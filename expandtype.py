from mtypes import (
    Typ, Instance, Callable, TypeVisitor, UnboundType, ErrorType, Any, Void,
    NoneTyp, TypeVar, Overloaded, TupleType
)


def expand_type( typ, map):
    """Expand any type variable references in a type with the actual values of
    type variables in an instance.
    """
    return typ.accept(ExpandTypeVisitor(map))


def expand_type_by_instance( typ, instance):
    """Expand type variables in type using type variable values in an
    instance."""
    if instance.args == []:
        return typ
    else:
        variables = {}
        for i in range(len(instance.args)):
            variables[i + 1] = instance.args[i]
        typ = expand_type(typ, variables)
        if isinstance(typ, Callable):
            bounds = []
            for j in range(len(instance.args)):
                bounds.append((j + 1, instance.args[j]))
            typ = update_callable_implicit_bounds(typ, bounds)
        else:
            pass
        return typ


class ExpandTypeVisitor(TypeVisitor):
    variables = None  # Lower bounds
    
    def __init__(self, variables):
        self.variables = variables
    
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
        args = self.expand_types(t.args)
        return Instance(t.typ, args, t.line, t.repr)
    
    def visit_type_var(self, t):
        repl = self.variables.get(t.id, t)
        if isinstance(repl, Instance):
            inst = repl
            # Return copy of instance with type erasure flag on.
            return Instance(inst.typ, inst.args, inst.line, inst.repr, True)
        else:
            return repl
    
    def visit_callable(self, t):
        return Callable(self.expand_types(t.arg_types),
                        t.arg_kinds,
                        t.arg_names,
                        t.ret_type.accept(self),
                        t.is_type_obj(),
                        t.name,
                        t.variables,
                        self.expand_bound_vars(t.bound_vars), t.line, t.repr)
    
    def visit_overloaded(self, t):
        items = []
        for item in t.items():
            items.append(item.accept(self))
        return Overloaded(items)
    
    def visit_tuple_type(self, t):
        return TupleType(self.expand_types(t.items), t.line, t.repr)
    
    def expand_types(self, types):
        a = []
        for t in types:
            a.append(t.accept(self))
        return a
    
    def expand_bound_vars(self, types):
        a = []
        for id, t in types:
            a.append((id, t.accept(self)))
        return a


def update_callable_implicit_bounds(t, arg_types):
    # FIX what if there are existing bounds?
    return Callable(t.arg_types,
                    t.arg_kinds,
                    t.arg_names,
                    t.ret_type,
                    t.is_type_obj(),
                    t.name,
                    t.variables,
                    arg_types, t.line, t.repr)


def expand_caller_var_args( arg_types, fixed_argc):
    """Expand the caller argument types in a varargs call. Fixedargc
    is the maximum number of fixed arguments that the target function
    accepts.
    
    Return (fixed argument types, type of the rest of the arguments). Return
    (nil, nil) if the last (vararg) argument had an invalid type. If the vararg
    argument was not an array (nor dynamic), the last item in the returned
    tuple is nil.
    """
    if isinstance(arg_types[-1], TupleType):
        return arg_types[:-1] + (arg_types[-1]).items, None
    else:
        item_type = None
        if isinstance(arg_types[-1], Any):
            item_type = Any()
        elif isinstance(arg_types[-1], Instance) and (
                (arg_types[-1]).typ.full_name() == 'builtins.list'):
            # List.
            item_type = (arg_types[-1]).args[0]
        else:
            return None, None
        
        if len(arg_types) > fixed_argc:
            return arg_types[:-1], item_type
        else:
            return (arg_types[:-1] +
                    [item_type] * (fixed_argc - len(arg_types) + 1), item_type)
