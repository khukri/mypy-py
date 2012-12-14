import checker
from join import is_similar_callables, combine_similar_callables
from mtypes import (
    Typ, Any, TypeVisitor, UnboundType, Void, ErrorType, NoneTyp, TypeVar,
    Instance, Callable, TupleType, ErasedType
)
from sametypes import is_same_type
from subtypes import is_subtype


def meet_types(s, t, basic):
    if isinstance(s, Any) or isinstance(s, ErasedType):
        return s
    
    return t.accept(TypeMeetVisitor(s, basic))


class TypeMeetVisitor(TypeVisitor):
    def __init__(self, s, basic):
        self.s = s
        self.basic = basic
    
    def visit_unbound_type(self, t):
        if isinstance(self.s, Void) or isinstance(self.s, ErrorType):
            return ErrorType()
        elif isinstance(self.s, NoneTyp):
            return self.s
        else:
            return Any()
    
    def visit_error_type(self, t):
        return t
    
    def visit_any(self, t):
        return t
    
    def visit_void(self, t):
        if isinstance(self.s, Void):
            return t
        else:
            return ErrorType()
    
    def visit_none_type(self, t):
        if not isinstance(self.s, Void) and not isinstance(self.s, ErrorType):
            return t
        else:
            return ErrorType()

    def visit_erased_type(self, t):
        return self.s
    
    def visit_type_var(self, t):
        if isinstance(self.s, TypeVar) and (self.s).id == t.id:
            return self.s
        else:
            return self.default(self.s)
    
    def visit_instance(self, t):
        if isinstance(self.s, Instance):
            si = self.s
            if t.typ == si.typ:
                if is_subtype(t, self.s):
                    # Combine type arguments. We could have used join below
                    # equivalently.
                    args = []
                    for i in range(len(t.args)):
                        args.append(self.meet(t.args[i], si.args[i]))
                    return Instance(t.typ, args)
                else:
                    return NoneTyp()
            else:
                if is_subtype(t, self.s):
                    return t
                elif is_subtype(self.s, t):
                    # See also above comment.
                    return self.s
                else:
                    return NoneTyp()
        else:
            return self.default(self.s)
    
    def visit_callable(self, t):
        if isinstance(self.s, Callable) and is_similar_callables(
                                                        t, self.s):
            return combine_similar_callables(t, self.s, self.basic)
        else:
            return self.default(self.s)
    
    def visit_tuple_type(self, t):
        if isinstance(self.s, TupleType) and ((self.s).length() ==
                                              t.length()):
            items = []
            for i in range(t.length()):
                items.append(self.meet(t.items[i],
                                       (self.s).items[i]))
            return TupleType(items)
        else:
            return self.default(self.s)
    
    def visit_intersection(self, t):
        # Only support very rudimentary meets between intersection types.
        if is_same_type(self.s, t):
            return self.s
        else:
            return self.default(self.s)
    
    def meet(self, s, t):
        return meet_types(s, t, self.basic)
    
    def default(self, typ):
        if isinstance(typ, UnboundType):
            return Any()
        elif isinstance(typ, Void) or isinstance(typ, ErrorType):
            return ErrorType()
        else:
            return NoneTyp()
