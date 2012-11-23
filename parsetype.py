from mtypes import Typ, TypeVars, TypeVarDef, Any, Void, UnboundType
from typerepr import (
    TypeVarsRepr, TypeVarDefRepr, AnyRepr, VoidRepr, CommonTypeRepr,
    ListTypeRepr
)
from lex import Token, Name


none = Token('') # Empty token


def parse_type( tok, index):
    """Parse a type. Return (type, index after type)."""
    p = TypeParser(tok, index)
    return p.parse_type(), p.index()


def parse_type_variables( tok, index, is_func):
    """Parse type variables and optional bounds (<...>). Return (bounds, index
    after bounds).
    """
    p = TypeParser(tok, index)
    return p.parse_type_variables(is_func), p.index()


def parse_type_args( tok, index):
    """Parse type arguments within angle brackets (<...>). Return
    (types, < token, > token, comma tokens, token index after >).
    """
    p = TypeParser(tok, index)
    types, lparen, rparen, commas = p.parse_type_args()
    return types, lparen, rparen, commas, p.index()


class TypeParser:
    tok = None
    ind = None
    # Have we consumed only the first '>' of a '>>' token?
    partial_shr = None
    
    def __init__(self, tok, ind):
        self.tok = tok
        self.ind = ind
        self.partial_shr = False
    
    def index(self):
        return self.ind
    
    def parse_type(self):
        """Parse a type."""
        t = self.current_token()
        if t.string == 'any':
            return self.parse_any_type()
        elif t.string == 'void':
            return self.parse_void_type()
        elif isinstance(t, Name):
            return self.parse_named_type()
        else:
            self.parse_error()
    
    def parse_type_variables(self, is_func):
        """Parse type variables and optional bounds (<...>)."""
        langle = self.expect('<')
        
        commas = []
        items = []
        n = 1
        while True:
            t = self.parse_type_variable(n, is_func)
            items.append(t)
            if self.current_token_str() != ',':
                break
            commas.append(self.skip())
            n += 1
        
        rangle = self.expect('>')
        return TypeVars(items, TypeVarsRepr(langle, commas, rangle))
    
    def parse_type_variable(self, n, is_func):
        t = self.expect_type(Name)
        
        line = t.line
        name = t.string
        
        is_tok = none
        bound = None
        if self.current_token_str() == 'is':
            is_tok = self.skip()
            bound = self.parse_type()
        
        if is_func:
            n = -n
        
        return TypeVarDef(name, n, bound, line, TypeVarDefRepr(t, is_tok))
    
    def parse_type_args(self):
        """Parse type arguments within angle brackets (<...>)."""
        langle = self.expect('<')
        commas = []
        types = []
        while True:
            types.append(self.parse_type())
            if self.current_token_str() != ',':
                break
            commas.append(self.skip())
        rangle = self.expect('>')
        return types, langle, rangle, commas
    
    def parse_any_type(self):
        """Parse 'any' type (or list of ... of any)."""
        tok = self.skip()
        anyt = Any(tok.line, AnyRepr(tok))
        return self.parse_optional_list_type(anyt)
    
    def parse_void_type(self):
        """Parse 'void' type."""
        tok = self.skip()
        return Void(None, tok.line, VoidRepr(tok))
    
    def parse_named_type(self):
        line = self.current_token().line
        name = ''
        components = []
        
        components.append(self.expect_type(Name))
        name += components[-1].string
        
        while self.current_token_str() == '.':
            components.append(self.skip())
            t = self.expect_type(Name)
            components.append(t)
            name += '.' + t.string
        
        langle, rangle = none, none
        commas = []
        args = []
        if self.current_token_str() == '<':
            langle = self.skip()
            
            while True:
                typ = self.parse_type()
                args.append(typ)
                if self.current_token_str() != ',':
                    break
                commas.append(self.skip())
            
            rangle = self.expect('>')
        
        typ = UnboundType(name, args, line, CommonTypeRepr(components,
                                                           langle,
                                                           commas, rangle))
        return self.parse_optional_list_type(typ)

    def parse_optional_list_type(self, typ):
        """Try to parse list types t[]."""
        while self.current_token_str() == '[':
            line = self.current_token().line
            # TODO representation
            lbracket = self.skip()
            rbracket = self.expect(']')
            typ = UnboundType('__builtins__.list', [typ], line,
                              ListTypeRepr(lbracket, rbracket))
        return typ
    
    # Helpers
    
    def skip(self):
        self.ind += 1
        return self.tok[self.ind - 1]
    
    def expect(self, string):
        if string == '>' and self.partial_shr:
            self.partial_shr = False
            self.ind += 1
            return Token('')
        elif string == '>' and self.tok[self.ind].string == '>>':
            self.partial_shr = True
            return self.tok[self.ind]
        elif self.partial_shr:
            self.parse_error()
        elif self.tok[self.ind].string == string:
            self.ind += 1
            return self.tok[self.ind - 1]
        else:
            self.parse_error()
    
    def expect_type(self, typ):
        if self.partial_shr:
            self.parse_error()
        elif isinstance(self.current_token(), typ):
            self.ind += 1
            return self.tok[self.ind - 1]
        else:
            self.parse_error()
    
    def current_token(self):
        return self.tok[self.ind]
    
    def current_token_str(self):
        s = self.current_token().string
        if s == '>>':
            s = '>'
        return s
    
    def parse_error(self):
        raise TypeParseError(self.tok, self.ind)


class TypeParseError(Exception):
    index = None
    token = None
    
    def __init__(self, token, index):
        self.token = token[index]
        self.index = index
        super().__init__()
