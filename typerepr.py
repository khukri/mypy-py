"""Representation classes for Typ subclasses and TypeVars.

These are used for source-source transformation that preserves original
formatting and comments.
"""

from lex import Token


class CommonTypeRepr:
    """Representation of UnboundType, Instance and Callable."""
    def __init__(self, components,  langle, commas, rangle):
        # Note: langle and rangle may be empty.
        self.components = components
        self.langle = langle
        self.commas = commas
        self.rangle = rangle


class ListTypeRepr:
    """Representation of list type t[]."""
    def __init__(self, lbracket, rbracket):
        self.lbracket = lbracket
        self.rbracket = rbracket


class AnyRepr:
    """Representation of Any."""
    def __init__(self, any_tok):
        self.any_tok = any_tok


class VoidRepr:
    """Representation of Void."""
    def __init__(self, void):
        self.void = void


class CallableRepr:
    """Representation of Callable."""
    def __init__(self, func, langle, lparen, commas, rparen, rangle):
        self.func = func
        self.langle = langle
        self.lparen = lparen
        self.commas = commas
        self.rparen = rparen
        self.rangle = rangle


class TypeVarRepr:
    """Representation of TypeVar."""
    def __init__(self, name):
        self.name = name


class TypeVarsRepr:
    """Representation of TypeVars."""
    def __init__(self, langle, commas, rangle):
        self.langle = langle
        self.commas = commas
        self.rangle = rangle


class TypeVarDefRepr:
    """Representation of TypeVarDef."""
    def __init__(self, name, is_tok):
        # TODO remove is_tok
        self.name = name
        self.is_tok = is_tok
