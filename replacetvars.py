from lex import Token
from mtypes import Typ, Any, NoneTyp, TypeTranslator, TypeVar
from typerepr import AnyRepr


def replace_type_vars( typ, func_tvars=True):
    """Replace type variable references in a type with the any type. If
    func_tvars is false, only replace instance type variables.
    """
    return typ.accept(ReplaceTypeVarsVisitor(func_tvars))


class ReplaceTypeVarsVisitor(TypeTranslator):
    # Only override type variable handling; otherwise perform an indentity
    # transformation.
    
    func_tvars = None
    
    def __init__(self, func_tvars):
        self.func_tvars = func_tvars
    
    def visit_type_var(self, t):
        if t.id > 0 or self.func_tvars:
            if t.repr is not None:
                # Give a representation for the dynamic type.
                tok = Token('any')
                tok.pre = t.repr.name.pre
                return Any(t.line, AnyRepr(tok))
            else:
                return Any()
        else:
            return t


def replace_func_type_vars( typ, target_type):
    """Replace function type variables in a type with the target type."""
    return typ.accept(ReplaceFuncTypeVarsVisitor(target_type))


class ReplaceFuncTypeVarsVisitor(TypeTranslator):
    def __init__(self, target_type):
        self.target_type = target_type
    
    def visit_type_var(self, t):
        if t.id < 0:
            return self.target_type
        else:
            return t
