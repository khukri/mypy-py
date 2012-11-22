from mtypes import (
    Typ, TypeVisitor, UnboundType, ErrorType, Any, Void, NoneTyp, Instance,
    TypeVar, Callable, TupleType
)
import checker


def erase_type( typ, basic):
    """Erase any type variables from a type. Also replace complex types (tuple,
    function) with the corresponding concrete types.
    
    Examples:
      A -> A
      B<X> -> B<any>
      tuple<A, B> -> tuple
      func<...> -> function
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
        return self.basic.function
    
    def visit_tuple_type(self, t):
        return self.basic.tuple