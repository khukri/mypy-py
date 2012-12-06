"""Classes for storing the lexical token information of nodes.

This is used for outputting the original source code represented by the nodes
(including original formatting and comments).

Each node representation usually only contains tokens directly associated
with that node (terminals). All members are Tokens or lists of Tokens,
unless explicitly mentioned otherwise.

If a representation has a Break token, the member name is br.
"""

from lex import Token


class MypyFileRepr:
    def __init__(self, eof):
        self.eof = eof


class ImportRepr:
    def __init__(self, import_tok, components, as_names, commas, br):
        self.import_tok = import_tok
        self.components = components
        self.as_names = as_names
        self.commas = commas
        self.br = br


class ImportFromRepr:
    def __init__(self, from_tok, components, import_tok, lparen, names, rparen, br):
        # Notes:
        # - lparen and rparen may be empty
        # - in each names tuple, the first item contains tokens for
        #   'name [as name]' and the second item is a comma or empty.
        self.from_tok = from_tok
        self.components = components
        self.import_tok = import_tok
        self.lparen = lparen
        self.names = names
        self.rparen = rparen
        self.br = br


class FuncRepr:
    def __init__(self, def_tok, name, args):
        # Note: name may be empty.
        self.def_tok = def_tok
        self.name = name
        self.args = args


class FuncArgsRepr:
    """Representation of a set of function arguments."""
    def __init__(self, lseparator, rseparator, arg_names, commas, assigns, asterisk):
        # Lseparator and rseparator are '(' and ')', respectively.
        self.lseparator = lseparator
        self.rseparator = rseparator
        self.arg_names = arg_names
        self.commas = commas
        self.assigns = assigns
        self.asterisk = asterisk


class VarRepr:
    def __init__(self, name, comma):
        # Note_ comma may be empty.
        self.name = name
        self.comma = comma


class TypeDefRepr:
    def __init__(self, class_tok, name, lparen, commas, rparen):
        self.class_tok = class_tok
        self.name = name
        self.lparen = lparen
        self.commas = commas
        self.rparen = rparen


class VarDefRepr:
    def __init__(self, assign, br):
        # Note: assign may be empty.
        self.assign = assign
        self.br = br


class DecoratorRepr:
    def __init__(self, at, br):
        self.at = at
        self.br = br


class BlockRepr:
    def __init__(self, colon, br, indent, dedent):
        self.colon = colon
        self.br = br
        self.indent = indent
        self.dedent = dedent


class GlobalDeclRepr:
    def __init__(self, global_tok, names, commas, br):
        self.global_tok = global_tok
        self.names = names
        self.commas = commas
        self.br = br


class ExpressionStmtRepr:
    def __init__(self, br):
        self.br = br


class AssignmentStmtRepr:
    def __init__(self, assigns, br):
        self.assigns = assigns
        self.br = br


class OperatorAssignmentStmtRepr:
    def __init__(self, assign, br):
        self.assign = assign
        self.br = br


class WhileStmtRepr:
    def __init__(self, while_tok, else_tok):
        self.while_tok = while_tok
        self.else_tok = else_tok


class ForStmtRepr:
    def __init__(self, for_tok, commas, in_tok, else_tok):
        self.for_tok = for_tok
        self.commas = commas
        self.in_tok = in_tok
        self.else_tok = else_tok


class SimpleStmtRepr:
    """Representation for break, continue, pass, return and assert."""
    def __init__(self, keyword, br):
        self.keyword = keyword
        self.br = br


class IfStmtRepr:
    def __init__(self, if_tok, elif_toks, else_tok):
        # Note: else_tok may be empty.
        self.if_tok = if_tok
        self.elif_toks = elif_toks
        self.else_tok = else_tok


class RaiseStmtRepr:
    def __init__(self, raise_tok, from_tok, br):
        self.raise_tok = raise_tok
        self.from_tok = from_tok
        self.br = br


class TryStmtRepr:
    try_tok = None
    except_toks = None  # Token[]
    name_toks = None    # Token[], may be empty
    as_toks = None      # Token[], may be empty
    else_tok = None
    finally_tok = None
    
    def __init__(self, try_tok, except_toks, name_toks, as_toks, else_tok, finally_tok):
        self.try_tok = try_tok
        self.except_toks = except_toks
        self.name_toks = name_toks
        self.as_toks = as_toks
        self.else_tok = else_tok
        self.finally_tok = finally_tok


class WithStmtRepr:
    def __init__(self, with_tok, as_toks, commas):
        self.with_tok = with_tok
        self.as_toks = as_toks
        self.commas = commas


class IntExprRepr:
    def __init__(self, int):
        self.int = int


class StrExprRepr:
    def __init__(self, string):
        self.string = string


class FloatExprRepr:
    def __init__(self, float):
        self.float = float


class ParenExprRepr:
    def __init__(self, lparen, rparen):
        self.lparen = lparen
        self.rparen = rparen


class NameExprRepr:
    def __init__(self, id):
        self.id = id


class MemberExprRepr:
    def __init__(self, dot, name):
        self.dot = dot
        self.name = name


class CallExprRepr:
    def __init__(self, lparen, commas, star, star2, keywords, rparen):
        # Asterisk may be empty.
        self.lparen = lparen
        self.commas = commas
        self.star = star
        self.star2 = star2
        self.keywords = keywords
        self.rparen = rparen


class IndexExprRepr:
    def __init__(self, lbracket, rbracket):
        self.lbracket = lbracket
        self.rbracket = rbracket


class SliceExprRepr:
    def __init__(self, colon, colon2):
        self.colon = colon
        self.colon2 = colon2


class UnaryExprRepr:
    def __init__(self, op):
        self.op = op


class OpExprRepr:
    def __init__(self, op, op2):
        # Note: op2 may be empty; it is used for "is not" and "not in".
        self.op = op
        self.op2 = op2


class CastExprRepr:
    def __init__(self, lparen, rparen):
        self.lparen = lparen
        self.rparen = rparen


class FuncExprRepr:
    def __init__(self, lambda_tok, colon, args):
        self.lambda_tok = lambda_tok
        self.colon = colon
        self.args = args


class SuperExprRepr:
    def __init__(self, super_tok, lparen, rparen, dot, name):
        self.super_tok = super_tok
        self.lparen = lparen
        self.rparen = rparen
        self.dot = dot
        self.name = name


class ListExprRepr:
    def __init__(self, lbracket, commas, rbracket, langle, rangle):
        self.lbracket = lbracket
        self.commas = commas
        self.rbracket = rbracket
        self.langle = langle
        self.rangle = rangle


class TupleExprRepr:
    def __init__(self, lparen, commas, rparen):
        # Note: lparen and rparen may be empty.
        self.lparen = lparen
        self.commas = commas
        self.rparen = rparen


class DictExprRepr:
    def __init__(self, lbrace, colons, commas, rbrace, langle, type_comma, rangle):
        self.lbrace = lbrace
        self.colons = colons
        self.commas = commas
        self.rbrace = rbrace
        self.langle = langle
        self.type_comma = type_comma
        self.rangle = rangle


class AnnotationRepr: pass


class TypeApplicationRepr:
    def __init__(self, langle, commas, rangle):
        self.langle = langle
        self.commas = commas
        self.rangle = rangle


class GeneratorExprRepr:
    def __init__(self, for_tok, commas, in_tok, if_tok):
        self.for_tok = for_tok
        self.commas = commas
        self.in_tok = in_tok
        self.if_tok = if_tok


class ListComprehensionRepr:
    def __init__(self, lbracket, rbracket):
        self.lbracket = lbracket
        self.rbracket = rbracket
