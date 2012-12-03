from mtypes import (
    Typ, UnboundType, TypeVar, TupleType, Instance, Any, Callable, TypeVars,
    Void, NoneTyp, TypeVarDef, TypeVisitor
)
from typerepr import TypeVarRepr
from nodes import GDEF, TypeInfo, Context, SymbolTableNode, TVAR
import nodes


class TypeAnalyser(TypeVisitor):
    """Semantic analyzer for types."""
    lookup = None
    fail = None
    
    def __init__(self, lookup_func, fail_func):
        self.lookup = lookup_func
        self.fail = fail_func
    
    def visit_unbound_type(self, t):
        if t.name == 'func':
            return self.anal_function_type(t)
        sym = self.lookup(t.name, t)
        if sym is not None:
            if sym.kind == TVAR:
                if len(t.args) > 0:
                    self.fail('Type variable "{}" used with arguments'.format(
                        t.name), t)
                return TypeVar(t.name, sym.tvar_id, False, t.line,
                               TypeVarRepr(t.repr.components[0]))
            elif sym.kind != GDEF or not isinstance(sym.node, TypeInfo):
                name = sym.full_name()
                if name is None:
                    name = sym.node.name()
                self.fail('Invalid type "{}"'.format(name), t)
                return t
            info = sym.node
            if len(t.args) > 0 and info.full_name() == 'builtins.tuple':
                return TupleType(self.anal_array(t.args), t.line, t.repr)
            elif len(t.args) != len(info.type_vars):
                if len(t.args) == 0:
                    # Implicit 'any' type arguments.
                    # TODO remove <Typ> below
                    return Instance(sym.node, [Any()] * len(info.type_vars),
                                    t.line, t.repr)
                # Invalid number of type parameters.
                n = len((sym.node).type_vars)
                s = '{} type arguments'.format(n)
                if n == 0:
                    s = 'no type arguments'
                elif n == 1:
                    s = '1 type argument'
                act = str(len(t.args))
                if act == '0':
                    act = 'none'
                self.fail('"{}" expects {}, but {} given'.format(
                    (sym.node).name(), s, act), t)
                return t
            else:
                # Ok; analyze arguments and construct Instance type. Upper
                # bounds are never present at this stage, as they are only used
                # during type inference.
                return Instance(sym.node, self.anal_array(t.args),
                                t.line, t.repr)
        else:
            return t
    
    def anal_function_type(self, t):
        a = []
        for at in t.args[:-1]:
            a.append(at.accept(self))
        return Callable(a, [nodes.ARG_POS] * len(a), [None] * len(a),
                        t.args[-1].accept(self),
                        False,
                        None,
                        TypeVars([]), [], t.line, t.repr)
    
    def visit_any(self, t):
        return t
    
    def visit_void(self, t):
        return t
    
    def visit_none_type(self, t):
        return t
    
    def visit_instance(self, t):
        raise RuntimeError('Instance is already analysed')
    
    def visit_type_var(self, t):
        raise RuntimeError('TypeVar is already analysed')
    
    def visit_callable(self, t):
        res = Callable(self.anal_array(t.arg_types),
                       t.arg_kinds,
                       t.arg_names,
                       t.ret_type.accept(self),
                       t.is_type_obj(),
                       t.name,
                       self.anal_var_defs(t.variables),
                       self.anal_bound_vars(t.bound_vars), t.line, t.repr)
        
        return res
    
    def visit_tuple_type(self, t):
        return TupleType(self.anal_array(t.items), t.line, t.repr)
    
    def anal_array(self, a):
        res = []
        for t in a:
            res.append(t.accept(self))
        return res
    
    def anal_bound_vars(self, a):
        res = []
        for id, t in a:
            res.append((id, t.accept(self)))
        return res
    
    def anal_var_defs(self, var_defs):
        a = []
        for vd in var_defs.items:
            bound = None
            if vd.bound is not None:
                bound = vd.bound.accept(self)
            a.append(TypeVarDef(vd.name, vd.id, bound, vd.line, vd.repr))
        return TypeVars(a, var_defs.repr)
