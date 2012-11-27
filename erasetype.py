from mtypes import (
    Typ, TypeVisitor, UnboundType, ErrorType, Any, Void, NoneTyp, Instance,
    TypeVar, Callable, TupleType, Overloaded
)
import checker


def erase_type( typ, basic):
    """Erase any type variables from a type.

    Also replace tuple types with the corresponding concrete types. Replace
    callable types with empty callable types.
    
    Examples:
      A -> A
      B<X> -> B<any>
      tuple<A, B> -> tuple
      func<...> -> func<void>
      """
    return typ.accept(EraseTypeVisitor(basic))


class EraseTypeVisitor(TypeVisitor):
    def __init__(self, basic):
        self.basic = basic
    
    def visit_unbound_type(self, t):
        return t
    
    def visit_error_type(self, t):
        return t
    
    def visit_any(self, t):
        return t
    
    def visit_void(self, t):
        return t
    
    def visit_none_type(self, t):
        return t
    
    def visit_instance(self, t):
        return Instance(t.typ, [Any()] * len(t.args), t.line, t.repr)
    
    def visit_type_var(self, t):
        return Any()
    
    def visit_callable(self, t):
        # We must preserve the type object flag for overload resolution to
        # work.
        return Callable([], 0, False, Void(), t.is_type_obj())

    def visit_overloaded(self, t):
        return t.items()[0].accept(self)
    
    def visit_tuple_type(self, t):
        return self.basic.tuple
