from mtypes import (
    Typ, UnboundType, ErrorType, Any, NoneTyp, Void, TupleType, Callable,
    TypeVar, Instance, TypeVisitor
)


def is_same_type( left, right):
    """Is 'left' the same type as 'right'?"""
    if isinstance(right, UnboundType):
        # Make unbound types same as anything else to reduce the number of
        # generated spurious error messages.
        return True
    else:
        return left.accept(SameTypeVisitor(right))


def is_same_types( a1, a2):
    if len(a1) != len(a2):
        return False
    for i in range(len(a1)):
        if not is_same_type(a1[i], a2[i]):
            return False
    return True


class SameTypeVisitor(TypeVisitor):
    """Visitor for checking whether two types are the 'same' type."""
    def __init__(self, right):
        self.right = right
    
    # visit_x(left) means: is left (which is an instance of X) the same type as
    # right?
    
    def visit_unbound_type(self, left):
        return True
    
    def visit_error_type(self, left):
        return False
    
    def visit_any(self, left):
        return isinstance(self.right, Any)
    
    def visit_void(self, left):
        return isinstance(self.right, Void)
    
    def visit_none_type(self, left):
        return isinstance(self.right, NoneTyp)
    
    def visit_instance(self, left):
        return (isinstance(self.right, Instance) and
                left.typ == (self.right).typ and
                is_same_types(left.args, (self.right).args))
    
    def visit_type_var(self, left):
        return (isinstance(self.right, TypeVar) and
                left.id == (self.right).id and
                left.is_wrapper_var == (self.right).is_wrapper_var)
    
    def visit_callable(self, left):
        # FIX generics
        if isinstance(self.right, Callable):
            cright = self.right
            return (is_same_type(left.ret_type, cright.ret_type) and
                    is_same_types(left.arg_types, cright.arg_types) and
                    left.min_args == cright.min_args and
                    left.is_var_arg == cright.is_var_arg and
                    left.is_type_obj() == cright.is_type_obj())
        else:
            return False
    
    def visit_tuple_type(self, left):
        if isinstance(self.right, TupleType):
            return is_same_types(left.items, (self.right).items)
        else:
            return False
