"""Abstract syntax tree node classes (i.e. parse tree)."""

import re

from lex import Token
from strconv import StrConv
from visitor import NodeVisitor
from util import dump_tagged, short_type


class Context:
    # Supertype for objects that are valid as error message locations.
    def get_line(self): pass


import mtypes


# Variable kind constants
LDEF = 0
GDEF = 1
MDEF = 2
MODULE_REF = 3
TVAR = 4 # Constant for type variable nodes in symbol table


node_kinds = {
    LDEF: 'Ldef',
    GDEF: 'Gdef',
    MDEF: 'Mdef',
    MODULE_REF: 'ModuleRef',
    TVAR: 'Tvar'
}


# Nodes that can be stored in the symbol table.
# TODO better name
# TODO remove or combine with SymNode?
class AccessorNode: pass


class SymNode:
    # Nodes that can be stored in a symbol table.
    # TODO do not use methods for these
    def name(self): pass
    def full_name(self): pass


class Node(Context):
    """Common base class for all non-type parse tree nodes."""
    
    line = -1
    repr = None # Textual representation
    
    def __str__(self):
        return self.accept(StrConv())
    
    def set_line(self, tok):
            
        def set_line1(self, tok):
            self.line = tok.line
            return self
        
        def set_line2(self, line):
            self.line = line
            return self
    
        if isinstance(tok, Token):
            return set_line1(self, tok)
        elif isinstance(tok, int):
            return set_line2(self, tok)
        else:
            raise TypeError("Invalid argument types")

    def get_line(self):
        # TODO this should be just 'line'
        return self.line
    
    def accept(self, visitor):
        raise RuntimeError('Not implemented')


class MypyFile(Node, AccessorNode, SymNode):
    """The abstract syntax tree of a single source file."""
    
    _name = None         # Module name ('__main__' for initial file)
    _full_name = None    # Qualified module name
    path = None          # Path to the file (None if not known)
    defs = None   # Global definitions and statements
    is_bom = None       # Is there a UTF-8 BOM at the start?
    names = None
    
    def __init__(self, defs, is_bom=False):
        self.defs = defs
        self.line = 1  # Dummy line number
        self.is_bom = is_bom

    def name(self):
        return self._name

    def full_name(self):
        return self._full_name
    
    def accept(self, visitor):
        return visitor.visit_mypy_file(self)


class Import(Node):
    ids = None     # (module id, as id)
    
    def __init__(self, ids):
        self.ids = ids
    
    def accept(self, visitor):
        return visitor.visit_import(self)


class ImportFrom(Node):
    id = None
    names = None  
    
    def __init__(self, id, names):
        self.id = id
        self.names = names
    
    def accept(self, visitor):
        return visitor.visit_import_from(self)


class ImportAll(Node):
    id = None
    
    def __init__(self, id):
        self.id = id
    
    def accept(self, visitor):
        return visitor.visit_import_all(self)


class FuncBase(Node, AccessorNode):
    typ = None   # Type signature (Callable or Overloaded)
    info = None    # If method, reference to TypeInfo
    def name(self):
        pass


class OverloadedFuncDef(FuncBase, SymNode):
    """A logical node representing all the overload variants of an overloaded
    function. This node has no explicit representation in the source program.
    Overloaded variants must be consecutive in the source file.
    """
    items = None
    _full_name = None
    
    def __init__(self, items):
        self.items = items
        self.set_line(items[0].line)
    
    def name(self):
        return self.items[1].name()

    def full_name(self):
        return self._full_name
    
    def accept(self, visitor):
        return visitor.visit_overloaded_func_def(self)


class FuncItem(FuncBase):
    # Fixed argument names
    args = None
    # Initialization expessions for fixed args; None if no initialiser
    init = None
    min_args = None           # Minimum number of arguments
    max_pos = None            # Maximum number of positional arguments, -1 if
                           # no explicit limit
    var_arg = None            # If not None, *x arg
    dict_var_arg = None       # If not None, **x arg
    body = None
    is_implicit = None    # Implicit dynamic types?
    is_overload = None    # Is this an overload variant of function with
                        # more than one overload variant?
    
    def __init__(self, args, init, var_arg, dict_var_arg, max_pos, body, typ=None):
        self.args = args
        self.var_arg = var_arg
        self.dict_var_arg = dict_var_arg
        self.max_pos = max_pos
        self.body = body
        self.typ = typ
        self.is_implicit = typ is None
        self.is_overload = False
        
        i2 = []
        self.min_args = 0
        for i in range(len(init)):
            if init[i] is not None:
                rvalue = init[i]
                lvalue = NameExpr(args[i].name()).set_line(rvalue.line)
                assign = AssignmentStmt([lvalue], rvalue)
                assign.set_line(rvalue.line)
                i2.append(assign)
            else:
                i2.append(None)
                if i < self.max_fixed_argc():
                    self.min_args = i + 1
        self.init = i2
    
    def max_fixed_argc(self):
        return len(self.args)
    
    def set_line(self, tok):
            
        def set_line1(self, tok):
            super().set_line(tok)
            for n in self.args:
                n.line = self.line
            return self
        
        def set_line2(self, tok):
            super().set_line(tok)
            for n in self.args:
                n.line = self.line
            return self
    
        if isinstance(tok, Token):
            return set_line1(self, tok)
        elif isinstance(tok, int):
            return set_line2(self, tok)
        else:
            raise TypeError("Invalid argument types")
    
    def init_expressions(self):
        res = []
        for i in self.init:
            if i is not None:
                res.append(i.rvalue)
            else:
                res.append(None)
        return res


class FuncDef(FuncItem, SymNode):
    _full_name = None      # Name with module prefix
    
    def __init__(self, name, args, init, var_arg, dict_var_arg, max_pos, body, typ=None):
        super().__init__(args, init, var_arg, dict_var_arg, max_pos, body, typ)
        self._name = name

    def name(self):
        return self._name
    
    def full_name(self):
        return self._full_name

    def accept(self, visitor):
        return visitor.visit_func_def(self)
    
    def is_constructor(self):
        return self.info is not None and self._name == '__init__'

    def get_name(self):
        """TODO merge with name()"""
        return self._name


class Decorator(Node):
    func = None       # FuncDef or Decorator
    decorator = None
    
    def __init__(self, func, decorator):
        self.func = func
        self.decorator = decorator
    
    def accept(self, visitor):
        return visitor.visit_decorator(self)


class Var(Node, AccessorNode, SymNode):
    _name = None       # Name without module prefix
    _full_name = None  # Name with module prefix
    is_init = None    # Is is initialized?
    info = None   # Defining class (for member variables)
    typ = None  # Declared type, or None if none
    is_self = None    # Is this the first argument to an ordinary method
                    # (usually "self")?
    
    def __init__(self, name):
        self._name = name
        self.is_init = False
        self.is_self = False

    def name(self):
        return self._name

    def full_name(self):
        return self._full_name
    
    def accept(self, visitor):
        return visitor.visit_var(self)


class TypeDef(Node):
    name = None        # Name of the class without module prefix
    full_name = None   # Fully qualified name of the class
    defs = None
    type_vars = None
    # Inherited types (Instance or UnboundType).
    base_types = None
    info = None    # Related TypeInfo
    is_interface = None
    
    def __init__(self, name, defs, type_vars=None, base_types=None, is_interface=False):
        if not base_types:
            base_types = []
        self.name = name
        self.defs = defs
        self.type_vars = type_vars
        self.base_types = base_types
        self.is_interface = is_interface
    
    def accept(self, visitor):
        return visitor.visit_type_def(self)
    
    def is_generic(self):
        return self.info.is_generic()


class VarDef(Node):
    items = None
    kind = None          # Ldef/Gdef/Mdef/...
    init = None         # Expression or None
    is_top_level = None # Is the definition at the top level (not within
                      # a function or a type)?
    is_init = None
    
    def __init__(self, items, is_top_level, init=None):
        self.items = items
        self.is_top_level = is_top_level
        self.init = init
        self.is_init = init is not None
    
    def info(self):
        return self.items[0][0].info
    
    def set_line(self, tok):
            
        def set_line1(self, tok):
            super().set_line(tok)
            for n, t in self.items:
                n.line = self.line
            return self
        
        def set_line2(self, tok):
            super().set_line(tok)
            for n, t in self.items:
                n.line = self.line
            return self
    
        if isinstance(tok, Token):
            return set_line1(self, tok)
        elif isinstance(tok, int):
            return set_line2(self, tok)
        else:
            raise TypeError("Invalid argument types")
    
    def accept(self, visitor):
        return visitor.visit_var_def(self)


class GlobalDecl(Node):
    names = None
    
    def __init__(self, names):
        self.names = names
    
    def accept(self, visitor):
        return visitor.visit_global_decl(self)


class Block(Node):
    body = None
    
    def __init__(self, body):
        self.body = body
    
    def accept(self, visitor):
        return visitor.visit_block(self)


# Statements


class ExpressionStmt(Node):
    expr = None
    
    def __init__(self, expr):
        self.expr = expr
    
    def accept(self, visitor):
        return visitor.visit_expression_stmt(self)


class AssignmentStmt(Node):
    lvalues = None
    rvalue = None
    
    def __init__(self, lvalues, rvalue):
        self.lvalues = lvalues
        self.rvalue = rvalue
    
    def accept(self, visitor):
        return visitor.visit_assignment_stmt(self)


class OperatorAssignmentStmt(Node):
    op = None
    lvalue = None
    rvalue = None
    
    def __init__(self, op, lvalue, rvalue):
        self.op = op
        self.lvalue = lvalue
        self.rvalue = rvalue
    
    def accept(self, visitor):
        return visitor.visit_operator_assignment_stmt(self)


class WhileStmt(Node):
    expr = None
    body = None
    else_body = None
    
    def __init__(self, expr, body, else_body):
        self.expr = expr
        self.body = body
        self.else_body = else_body
    
    def accept(self, visitor):
        return visitor.visit_while_stmt(self)


class ForStmt(Node):
    index = None   # Index variables
    types = None # Index variable types (each may be None)
    expr = None              # Expression to iterate
    body = None
    else_body = None
    
    def __init__(self, index, expr, body, else_body, types=None):
        self.index = index
        self.expr = expr
        self.body = body
        self.else_body = else_body
        self.types = types
    
    def accept(self, visitor):
        return visitor.visit_for_stmt(self)
    
    def is_annotated(self):
        ann = False
        for t in self.types:
            if t is not None:
                ann = True
        return ann


class ReturnStmt(Node):
    expr = None   # Expression or None
    
    def __init__(self, expr):
        self.expr = expr
    
    def accept(self, visitor):
        return visitor.visit_return_stmt(self)


class AssertStmt(Node):
    expr = None
    
    def __init__(self, expr):
        self.expr = expr
    
    def accept(self, visitor):
        return visitor.visit_assert_stmt(self)


class YieldStmt(Node):
    expr = None
    
    def __init__(self, expr):
        self.expr = expr
    
    def accept(self, visitor):
        return visitor.visit_yield_stmt(self)


class DelStmt(Node):
    expr = None
    
    def __init__(self, expr):
        self.expr = expr
    
    def accept(self, visitor):
        return visitor.visit_del_stmt(self)


class BreakStmt(Node):
    def accept(self, visitor):
        return visitor.visit_break_stmt(self)


class ContinueStmt(Node):
    def accept(self, visitor):
        return visitor.visit_continue_stmt(self)


class PassStmt(Node):
    def accept(self, visitor):
        return visitor.visit_pass_stmt(self)


class IfStmt(Node):
    expr = None
    body = None
    else_body = None
    
    def __init__(self, expr, body, else_body):
        self.expr = expr
        self.body = body
        self.else_body = else_body
    
    def accept(self, visitor):
        return visitor.visit_if_stmt(self)


class RaiseStmt(Node):
    expr = None
    from_expr = None
    
    def __init__(self, expr, from_expr=None):
        self.expr = expr
        self.from_expr = from_expr
    
    def accept(self, visitor):
        return visitor.visit_raise_stmt(self)


class TryStmt(Node):
    body = None                # Try body
    types = None          # Except type expressions
    vars = None            # Except variable names
    handlers = None      # Except bodies
    else_body = None
    finally_body = None
    
    def __init__(self, body, vars, types, handlers, else_body, finally_body):
        self.body = body
        self.vars = vars
        self.types = types
        self.handlers = handlers
        self.else_body = else_body
        self.finally_body = finally_body
    
    def accept(self, visitor):
        return visitor.visit_try_stmt(self)


class WithStmt(Node):
    expr = None
    name = None
    body = None
    
    def __init__(self, expr, name, body):
        self.expr = expr
        self.name = name
        self.body = body
    
    def accept(self, visitor):
        return visitor.visit_with_stmt(self)


# Expressions


class IntExpr(Node):
    """Integer literal"""
    value = None
    
    def __init__(self, value):
        self.value = value
    
    def accept(self, visitor):
        return visitor.visit_int_expr(self)


class StrExpr(Node):
    """String literal"""
    value = None
    
    def __init__(self, value):
        self.value = value
    
    def accept(self, visitor):
        return visitor.visit_str_expr(self)


class BytesExpr(Node):
    """Bytes literal"""
    value = None # TODO use bytes
    
    def __init__(self, value):
        self.value = value
    
    def accept(self, visitor):
        return visitor.visit_bytes_expr(self)


class FloatExpr(Node):
    """Float literal"""
    value = None
    
    def __init__(self, value):
        self.value = value
    
    def accept(self, visitor):
        return visitor.visit_float_expr(self)


class ParenExpr(Node):
    """Parenthesised expression"""
    expr = None
    
    def __init__(self, expr):
        self.expr = expr
    
    def accept(self, visitor):
        return visitor.visit_paren_expr(self)


class RefExpr(Node):
    """Abstract base class"""
    kind = None      # Ldef/Gdef/Mdef/... (None if not available)
    node = None     # Var, FuncDef or TypeInfo that describes this


class NameExpr(RefExpr):
    """Name expression

    This refers to a local name, global name or a module.
    """
    name = None      # Name referred to (may be qualified)
    full_name = None # Fully qualified name (or name if not global)
    info = None # TypeInfo of class surrounding expression (may be None)
    is_def = None   # Does this define a new variable as a lvalue?
    
    def __init__(self, name):
        self.name = name
        self.is_def = False
    
    def type_node(self):
        return self.node
    
    def accept(self, visitor):
        return visitor.visit_name_expr(self)


class MemberExpr(RefExpr):
    """Member access expression x.y"""
    expr = None
    name = None
    # Full name if referring to a name in module.
    full_name = None
    # True if first assignment to member via self in __init__ (and if not
    # defined in class body). After semantic analysis, this does not take base
    # classes into consideration at all; the type checker deals with these.
    is_def = False
    # The variable node related to a definition.
    def_var = None
    
    def __init__(self, expr, name):
        self.expr = expr
        self.name = name
    
    def accept(self, visitor):
        return visitor.visit_member_expr(self)


class CallExpr(Node):
    """Call expression"""
    callee = None
    args = None
    is_var_arg = None
    keyword_args = None
    dict_var_arg = None
    
    def __init__(self, callee, args, is_var_arg=False, keyword_args=None, dict_var_arg=None):
        if not keyword_args:
            keyword_args = []
        self.callee = callee
        self.args = args
        self.is_var_arg = is_var_arg
        self.keyword_args = keyword_args
        self.dict_var_arg = dict_var_arg
    
    def accept(self, visitor):
        return visitor.visit_call_expr(self)


class IndexExpr(Node):
    """Index expression x[y]"""
    base = None
    index = None
    
    def __init__(self, base, index):
        self.base = base
        self.index = index
    
    def accept(self, visitor):
        return visitor.visit_index_expr(self)


class UnaryExpr(Node):
    """Unary operation"""
    op = None
    expr = None
    
    def __init__(self, op, expr):
        self.op = op
        self.expr = expr
    
    def accept(self, visitor):
        return visitor.visit_unary_expr(self)


class OpExpr(Node):
    """Binary operation (other than . or [], which have specific nodes)"""
    op = None
    left = None
    right = None
    
    def __init__(self, op, left, right):
        self.op = op
        self.left = left
        self.right = right
    
    def accept(self, visitor):
        return visitor.visit_op_expr(self)


class SliceExpr(Node):
    """Slice expression (e.g. 'x:y', 'x:', '::2' or ':'); only valid
    as index in index expressions.
    """
    begin_index = None  # May be None
    end_index = None    # May be None
    stride = None       # May be None
    
    def __init__(self, begin_index, end_index, stride):
        self.begin_index = begin_index
        self.end_index = end_index
        self.stride = stride
    
    def accept(self, visitor):
        return visitor.visit_slice_expr(self)


class CastExpr(Node):
    expr = None
    typ = None
    
    def __init__(self, expr, typ):
        self.expr = expr
        self.typ = typ
    
    def accept(self, visitor):
        return visitor.visit_cast_expr(self)


class SuperExpr(Node):
    name = None
    info = None # Type that contains this super expression
    
    def __init__(self, name):
        self.name = name
    
    def accept(self, visitor):
        return visitor.visit_super_expr(self)


class FuncExpr(FuncItem):
    """Anonymous function expression"""
    def accept(self, visitor):
        return visitor.visit_func_expr(self)


class ListExpr(Node):
    """List literal expression [...] or <type> [...]"""
    items = None 
    typ = None # None if implicit type
    
    def __init__(self, items, typ=None):
        self.items = items
        self.typ = typ
    
    def accept(self, visitor):
        return visitor.visit_list_expr(self)


class DictExpr(Node):
    """Dictionary literal expression {key:value, ...} or <kt, vt> {...}."""
    items = None
    key_type = None    # None if implicit type
    value_type = None  # None if implicit type
    
    def __init__(self, items):
        self.items = items
    
    def accept(self, visitor):
        return visitor.visit_dict_expr(self)


class TupleExpr(Node):
    """Tuple literal expression (..., ...)"""
    items = None
    types = None
    
    def __init__(self, items, types=None):
        self.items = items
        self.types = types
    
    def accept(self, visitor):
        return visitor.visit_tuple_expr(self)


class SetExpr(Node):
    """Set literal expression {value, ...}."""
    items = None
    
    def __init__(self, items):
        self.items = items
    
    def accept(self, visitor):
        return visitor.visit_set_expr(self)


class GeneratorExpr(Node):
    """Generator expression ... for ... in ... [ if ... ]."""
    left_expr = None
    right_expr = None
    condition = None   # May be None
    index = None
    types = None
    
    def __init__(self, left_expr, index, types, right_expr, condition):
        self.left_expr = left_expr
        self.right_expr = right_expr
        self.condition = condition
        self.index = index
        self.types = types
    
    def accept(self, visitor):
        return visitor.visit_generator_expr(self)


class ListComprehension(Node):
    generator = None
    
    def __init__(self, generator):
        self.generator = generator
    
    def accept(self, visitor):
        return visitor.visit_list_comprehension(self)


class ConditionalExpr(Node):
    cond = None
    if_expr = None
    else_expr = None
    
    def __init__(self, cond, if_expr, else_expr):
        self.cond = cond
        self.if_expr = if_expr
        self.else_expr = else_expr
    
    def accept(self, visitor):
        return visitor.visit_conditional_expr(self)


class Annotation(Node):
    typ = None
    
    def __init__(self, typ, line=-1):
        self.typ = typ
        self.line = line
    
    def accept(self, visitor):
        return visitor.visit_annotation(self)


class TypeApplication(Node):
    expr = None   # Node
    types = None  # list<mtypes.Typ>
    
    def __init__(self, expr, types):
        self.expr = expr
        self.types = types
    
    def accept(self, visitor):
        return visitor.visit_type_application(self)


class CoerceExpr(Node):
    """Implicit coercion expression (used only when compiling/transforming;
    inserted after type checking).
    """
    expr = None
    target_type = None
    source_type = None
    is_wrapper_class = None
    
    def __init__(self, expr, target_type, source_type, is_wrapper_class):
        self.expr = expr
        self.target_type = target_type
        self.source_type = source_type
        self.is_wrapper_class = is_wrapper_class
    
    def accept(self, visitor):
        return visitor.visit_coerce_expr(self)


class JavaCast(Node):
    expr = None
    target = None
    
    def __init__(self, expr, target):
        self.expr = expr
        self.target = target
    
    def accept(self, visitor):
        return visitor.visit_java_cast(self)


class TypeExpr(Node):
    """Expression that evaluates to a runtime representation of a type. This is
    used only for runtime type checking. This node is always generated only
    after type checking.
    """
    typ = None
    
    def __init__(self, typ):
        self.typ = typ
    
    def accept(self, visitor):
        return visitor.visit_type_expr(self)


class TempNode(Node):
    """This node is not present in the original program; it is just an artifact
    of the type checker implementation. It only represents an opaque node with
    some fixed type.
    """
    typ = None
    
    def __init__(self, typ):
        self.typ = typ
    
    def accept(self, visitor):
        return visitor.visit_temp_node(self)


class TypeInfo(Node, AccessorNode, SymNode):
    """Class representing the type structure of a single class.

    The corresponding TypeDef instance represents the parse tree of
    the class.
    """
    _full_name = None     # Fully qualified name
    is_interface = None  # Is this a interface type?
    defn = None       # Corresponding TypeDef
    base = None   # Superclass or None (not interface)
    subtypes = None # Direct subclasses
    
    vars = None    # Member variables; also slots
    methods = None
    
    # TypeInfos of base interfaces
    interfaces = None
    
    # Information related to type annotations.
    
    # Generic type variable names
    type_vars = None
    
    # Type variable bounds (each may be None)
    # TODO implement these
    bounds = None
    
    # Inherited generic types (Instance or UnboundType or None). The first base
    # is the superclass, and the rest are interfaces.
    bases = None
    
    
    def __init__(self, vars, methods, defn):
        """Construct a TypeInfo."""
        self.subtypes = set()
        self.vars = {}
        self.methods = {}
        self.interfaces = []
        self.type_vars = []
        self.bounds = []
        self.bases = []
        self._full_name = defn.full_name
        self.is_interface = defn.is_interface
        self.vars = vars
        self.methods = methods
        self.defn = defn
        if defn.type_vars is not None:
            for vd in defn.type_vars.items:
                self.type_vars.append(vd.name)
    
    def name(self):
        """Short name."""
        return self.defn.name

    def full_name(self):
        return self._full_name
    
    def is_generic(self):
        """Is the type generic (i.e. does it have type variables)?"""
        return self.type_vars is not None and len(self.type_vars) > 0
    
    def set_type_bounds(self, a):
        for vd in a:
            self.bounds.append(vd.bound)
    
    
    # IDEA: Refactor the has* methods to be more consistent and document
    #       them.
    
    def has_readable_member(self, name):
        return self.has_var(name) or self.has_method(name)
    
    def has_writable_member(self, name):
        return self.has_var(name) or self.has_setter(name)
    
    def has_var(self, name):
        return self.get_var(name) is not None
    
    def has_method(self, name):
        return name in self.methods or (self.base is not None
                                        and self.base.has_method(name))
    
    def has_setter(self, name):
        # FIX implement
        return False
    
    
    def get_var(self, name):
        if name in self.vars:
            return self.vars[name]
        elif self.base is not None:
            return self.base.get_var(name)
        else:
            return None
    
    def get_var_or_getter(self, name):
        # TODO getter
        if name in self.vars:
            return self.vars[name]
        elif self.base is not None:
            return self.base.get_var_or_getter(name)
        else:
            return None
    
    def get_var_or_setter(self, name):
        # TODO setter
        if name in self.vars:
            return self.vars[name]
        elif self.base is not None:
            return self.base.get_var_or_setter(name)
        else:
            return None
    
    def get_method(self, name):
        if name in self.methods:
            return self.methods[name]
        elif self.base is not None:
            return self.base.get_method(name)
        else:
            return None
    
    
    def set_base(self, base):
        """Set the base class."""
        self.base = base
        base.subtypes.add(self)
    
    def has_base(self, full_name):
        """Return True if type has a basetype with the specified name,
        either via extension or via implementation.
        """
        if self.full_name() == full_name or (self.base is not None and
                                             self.base.has_base(full_name)):
            return True
        for iface in self.interfaces:
            if iface.full_name() == full_name or iface.has_base(full_name):
                return True
        return False
    
    def all_subtypes(self):
        """Return TypeInfos of all subtypes, including this type, as a set."""
        set = set([self])
        for subt in self.subtypes:
            for t in subt.all_subtypes():
                set.add(t)
        return set
    
    
    def add_interface(self, base):
        """Add a base interface."""
        self.interfaces.append(base)
    
    def all_directly_implemented_interfaces(self):
        """Return a list of interfaces that are either directly
        implemented by the type or that are the supertypes of other
        interfaces in the array.
        """
        # Interfaces never implement interfaces.
        if self.is_interface:
            return []
        a = []
        for i in range(len(self.interfaces)):
            iface = self.interfaces[i]
            if iface not in a:
                a.append(iface)
            ifa = iface
            while ifa.base is not None:
                ifa = ifa.base
                if ifa not in a:
                    a.append(ifa)
        return a
    
    def directly_implemented_interfaces(self):
        """Return a directly implemented interfaces.

        Omit inherited interfaces.
        """
        return self.interfaces[:]
    
    
    def __str__(self):
        """Return a string representation of the type, which includes the most
        important information about the type.
        """
        interfaces = []
        for i in self.interfaces:
            interfaces.append(i.full_name())
        base = None
        if self.base is not None:
            base = 'Base({})'.format(self.base.full_name())
        iface = None
        if self.is_interface:
            iface = 'Interface'
        return dump_tagged(['Name({})'.format(self.full_name()),
                            iface,
                            base,
                            ('Interfaces', interfaces),
                            ('Vars', self.vars.keys()),
                            ('Methods', self.methods.keys())],
                           'TypeInfo')


class SymbolTable(dict):
    def __str__(self):
        a = []
        for key, value in self.items():
            # Filter out the implicit import of builtins.
            if isinstance(value, SymbolTableNode):
                if (value.full_name() != 'builtins' and
                        not value.full_name().endswith('.__name__')):
                    a.append('  ' + str(key) + ' : ' + str(value))
            else:
                a.append('  <invalid item>')
        a = sorted(a)
        a.insert(0, 'SymbolTable(')
        a[-1] += ')'
        return '\n'.join(a)


class SymbolTableNode:
    kind = None      # Ldef/Gdef/Mdef/Tvar/...
    node = None  # Parse tree node of definition (FuncDef/Var/
                  # TypeInfo), None for Tvar
    tvar_id = None   # Type variable id (for Tvars only)
    mod_id = None    # Module id (e.g. "foo.bar") or None
    
    type_override = None  # If None, fall back to type of node
    
    def __init__(self, kind, node, mod_id=None, typ=None, tvar_id=0):
        self.kind = kind
        self.node = node
        self.type_override = typ
        self.mod_id = mod_id
        self.tvar_id = tvar_id
    
    def full_name(self):
        if self.node is not None:
            return self.node.full_name()
        else:
            return None
    
    def typ(self):
        # IDEA: Get rid of the any type.
        node = self.node
        if self.type_override is not None:
            return self.type_override
        elif ((isinstance(node, Var) or isinstance(node, FuncDef))
              and node.typ is not None):
            return node.typ.typ
        else:
            return None
    
    def __str__(self):
        s = '{}/{}'.format(node_kinds[self.kind], short_type(self.node))
        if self.mod_id is not None:
            s += ' ({})'.format(self.mod_id)
        # Include declared type of variables and functions.
        if self.typ() is not None:
            s += ' : {}'.format(self.typ())
        return s


def clean_up(s):
    return re.sub('.*::', '', s)
        

def function_type( func):
    if func.typ:
        return func.typ.typ
    else:
        # Implicit type signature with dynamic types.
        # Overloaded functions always have a signature, so func must be an
        # ordinary function.
        fdef = func        
        name = func.name()
        if name:
            name = '"{}"'.format(name)
        return mtypes.Callable( [mtypes.Any()] * len(fdef.args),
                               fdef.min_args,
                               fdef.var_arg is not None,
                               mtypes.Any(),
                               False,
                               name)


def method_type( func):
    """Return the signature of a method (omit self)."""
    t = function_type(func)
    if isinstance(t, mtypes.Callable):
        return method_callable(t)
    else:
        o = t
        it = []
        for c in o.items():
            it.append(method_callable(c))
        return mtypes.Overloaded(it)


def method_callable(c):
    return mtypes.Callable(c.arg_types[1:],
                           c.min_args - 1,
                           c.is_var_arg,
                           c.ret_type,
                           c.is_type_obj(),
                           c.name,
                           c.variables)
