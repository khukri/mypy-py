"""Mypy type checker."""

from errors import Errors
from nodes import (
    SymbolTable, Node, MypyFile, VarDef, LDEF, Var,
    OverloadedFuncDef, FuncDef, FuncItem, Annotation, FuncBase, TypeInfo,
    TypeDef, GDEF, Block, AssignmentStmt, NameExpr, MemberExpr, IndexExpr,
    TupleExpr, ListExpr, ParenExpr, ExpressionStmt, ReturnStmt, IfStmt,
    WhileStmt, OperatorAssignmentStmt, YieldStmt, WithStmt, AssertStmt,
    RaiseStmt, TryStmt, ForStmt, DelStmt, CallExpr, IntExpr, StrExpr,
    BytesExpr, FloatExpr, OpExpr, UnaryExpr, CastExpr, SuperExpr,
    TypeApplication, DictExpr, SliceExpr, FuncExpr, TempNode, SymbolTableNode,
    Context, AccessorNode, ListComprehension, ConditionalExpr, GeneratorExpr,
    Decorator, SetExpr
)
from nodes import function_type, method_type
from mtypes import (
    Typ, Any, Callable, Void, FunctionLike, Overloaded, TupleType, Instance,
    NoneTyp, UnboundType, TypeTranslator
)
from sametypes import is_same_type
from messages import MessageBuilder
import checkexpr
import messages
from subtypes import is_subtype, is_equivalent, map_instance_to_supertype
from semanal import self_type
from expandtype import expand_type_by_instance
from visitor import NodeVisitor


# Map from binary operator id to related method name.
op_methods = {
    '+': '__add__',
    '-': '__sub__',
    '*': '__mul__',
    '/': '__truediv__',
    '%': '__mod__',
    '//': '__floordiv__',
    '**': '__pow__',
    '&': '__and__',
    '|': '__or__',
    '^': '__xor__',
    '<<': '__lshift__',
    '>>': '__rshift__',
    '==': '__eq__',
    '!=': '__ne__',
    '<': '__lt__',
    '>=': '__ge__',
    '>': '__gt__',
    '<=': '__le__',
    'in': '__contains__'
}


class BasicTypes:
    """Collection of Instance types of basic types (object, type, etc.)."""
    def __init__(self, object, std_type, tuple, function):
        self.object = object
        self.std_type = std_type
        self.tuple = tuple
        self.function = function


class TypeChecker(NodeVisitor):
    """Mypy type checker.

    Type check mypy source files that have been semantically analysed.
    """
    
    errors = None          # Error reporting
    symtable = None   # Symbol table for the whole program
    msg = None     # Utility for generating messages
    type_map = None  # Types of type checked nodes
    expr_checker = None
    
    stack = None # Stack of local variable definitions
                    # None separates nested functions
    return_types = None   # Stack of function return types
    type_context = None   # Type context for type inference
    dynamic_funcs = None # Flags; true for dynamically typed functions
    
    globals = None
    class_tvars = None
    locals = None
    modules = None
    
    def __init__(self, errors, modules):
        """Construct a type checker. Use errors to report type check
        errors. Assume symtable has been populated by the semantic
        analyzer.
        """
        self.expr_checker
        self.errors = errors
        self.modules = modules
        self.msg = MessageBuilder(errors)
        self.type_map = {}
        self.expr_checker = checkexpr.ExpressionChecker(self, self.msg)
        self.stack = [None]
        self.return_types = []
        self.type_context = []
        self.dynamic_funcs = []
    
    def visit_file(self, file_node, path):  
        """Type check a mypy file with the given path."""
        self.errors.set_file(path)
        self.globals = file_node.names
        self.locals = None
        self.class_tvars = None
        
        for d in file_node.defs:
            self.accept(d)
    
    def accept(self, node, type_context=None):
        """Type check a node in the given type context."""
        self.type_context.append(type_context)
        typ = node.accept(self)
        self.type_context.pop()
        self.store_type(node, typ)
        if self.is_dynamic_function():
            return Any()
        else:
            return typ
    
    #
    # Definitions
    #
    
    def visit_var_def(self, defn):
        """Type check a variable definition (of any kind: local,
        member or local)."""
        # Type check initializer.
        if defn.init:
            # There is an initializer.
            if defn.items[0][1]:
                # Explicit types.
                if len(defn.items) == 1:
                    self.check_single_assignment(defn.items[0][1], None,
                                                 defn.init, defn.init)
                else:
                    # Multiple assignment.
                    lvt = []
                    for v, t in defn.items:
                        lvt.append(t)
                    self.check_multi_assignment(
                        lvt, [None] * len(lvt),
                        defn.init, defn.init)
            else:
                init_type = self.accept(defn.init)
                if defn.kind == LDEF and not defn.is_top_level:
                    # Infer local variable type if there is an initializer
                    # except if the# definition is at the top level (outside a
                    # function).
                    names = []
                    for vv, tt in defn.items:
                        names.append(vv)
                    self.infer_local_variable_type(names, init_type, defn)
        else:
            # No initializer
            if (defn.kind == LDEF and not defn.items[0][1] and
                    not defn.is_top_level and not self.is_dynamic_function()):
                self.fail(messages.NEED_ANNOTATION_FOR_VAR, defn)
    
    def infer_local_variable_type(self, x, y, z):
        # TODO
        raise RuntimeError('Not implemented')
    
    def visit_overloaded_func_def(self, defn):
        for fdef in defn.items:
            self.check_func_item(fdef)
        if defn.info:
            self.check_method_override(defn)
    
    def visit_func_def(self, defn):
        """Type check a function definition."""
        self.check_func_item(defn)
        if defn.info:
            self.check_method_override(defn)
    
    def check_func_item(self, defn):
        if defn.dict_var_arg:
            return self.msg.not_implemented('keyword arguments', defn)
        
        # We may be checking a function definition or an anonymous function. In
        # the first case, set up another reference with the precise type.
        fdef = None
        if isinstance(defn, FuncDef):
            fdef = defn
        
        self.dynamic_funcs.append(defn.typ is None)
        
        if fdef:
            self.errors.set_function(fdef.name())
        
        typ = function_type(defn)
        if isinstance(typ, Callable):
            self.check_func_def(defn, typ)
        else:
            raise RuntimeError('Not supported')
        
        if fdef:
            self.errors.set_function(None)
        
        self.dynamic_funcs.pop()
    
    def check_func_def(self, defn, typ):
        """Check a function definition."""
        # We may be checking a function definition or an anonymous function. In
        # the first case, set up another reference with the precise type.
        if isinstance(defn, FuncDef):
            fdef = defn
        else:
            fdef = None
        
        self.enter()
        
        if fdef:
            # The cast below will work since non-method create will cause
            # semantic analysis to fail, and type checking won't be done.
            if (fdef.info and fdef.name() == '__init__' and
                    not isinstance((typ).ret_type, Void) and
                    not self.dynamic_funcs[-1]):
                self.fail(messages.INIT_MUST_NOT_HAVE_RETURN_TYPE, defn.typ)
        
        # Push return type.
        self.return_types.append((typ).ret_type)
        
        # Add arguments to symbol table.
        ctype = typ
        nargs = len(defn.args)
        for i in range(len(ctype.arg_types)):
            arg_type = ctype.arg_types[i]
            if defn.var_arg and i == nargs:
                arg_type = self.named_generic_type('builtins.list', [arg_type])
                defn.var_arg.typ = Annotation(arg_type)
            else:
                defn.args[i].typ = Annotation(arg_type)
        
        # Type check initialization expressions.
        for j in range(len(defn.init)):
            if defn.init[j]:
                self.accept(defn.init[j])
        
        # Type check body.
        self.accept(defn.body)
        
        # Pop return type.
        self.return_types.pop()
        
        self.leave()
    
    def check_method_override(self, defn):
        """Check that function definition is compatible with any overridden
        definitions defined in superclasses or implemented interfaces.
        """
        # Check against definitions in superclass.
        self.check_method_or_accessor_override_for_base(defn, defn.info.base)
        # Check against definitions in implemented interfaces.
        for iface in defn.info.interfaces:
            self.check_method_or_accessor_override_for_base(defn, iface)
    
    def check_method_or_accessor_override_for_base(self, defn, base):
        """Check that function definition is compatible with any overridden
        definition in the specified supertype.
        """
        if base:
            if defn.name() != '__init__':
                # Check method override (create is special).
                base_method = base.get_method(defn.name())
                if base_method and base_method.info == base:
                    # There is an overridden method in the supertype.
                    
                    # Construct the type of the overriding method.
                    typ = method_type(defn)
                    # Map the overridden method type to subtype context so that
                    # it can be checked for compatibility. Note that multiple
                    # types from multiple implemented interface instances may
                    # be present.
                    original_type = map_type_from_supertype(
                        method_type(base_method), defn.info, base)
                    # Check that the types are compatible.
                    # TODO overloaded signatures
                    self.check_override(typ,
                                        original_type,
                                        defn.name(),
                                        base_method.info.name(),
                                        defn)
            
            # Also check interface implementations.
            for iface in base.interfaces:
                self.check_method_or_accessor_override_for_base(defn, iface)
            
            # We have to check that the member is compatible with all
            # supertypes due to the dynamic type. Otherwise we could first
            # override with dynamic and then with an arbitary type.
            self.check_method_or_accessor_override_for_base(defn, base.base)
    
    def check_override(self, override, original, name, supertype, node):
        """Check a method override with given signatures.

        Arguments:
          override:  The signature of the overriding method.
          original:  The signature of the original supertype method.
          name:      The name of the subtype. This and the next argument are
                     only used for generating error messages.
          supertype: The name of the supertype.
        """
        if (isinstance(override, Overloaded) or
                isinstance(original, Overloaded) or
                len((override).arg_types) !=
                    len((original).arg_types) or
                (override).min_args !=
                    (original).min_args):
            if not is_subtype(override, original):
                self.msg.signature_incompatible_with_supertype(
                    name, supertype, node)
            return
        else:
            # Give more detailed messages for the common case of both
            # signatures having the same number of arguments and no
            # intersection types.
            
            coverride = override
            coriginal = original
            
            for i in range(len(coverride.arg_types)):
                if not is_equivalent(coriginal.arg_types[i],
                                     coverride.arg_types[i]):
                    self.msg.argument_incompatible_with_supertype(
                        i + 1, name, supertype, node)
            
            if not is_subtype(coverride.ret_type, coriginal.ret_type):
                self.msg.return_type_incompatible_with_supertype(
                    name, supertype, node)
    
    def visit_type_def(self, defn):
        """Type check a type definition (class or interface)."""
        typ = self.lookup(defn.name, GDEF).node
        # TODO
        #addMembersToSymbolTable(type as TypeInfo)
        
        self.errors.set_type(defn.name, defn.is_interface)
        
        self.check_unique_interface_implementations(typ)
        
        self.check_interface_errors(typ)
        
        self.accept(defn.defs)
        
        self.errors.set_type(None, False)
    
    def check_unique_interface_implementations(self, typ):
        """Check that each interface is implemented only once."""
        ifaces = typ.interfaces[:]
        
        dup = find_duplicate(ifaces)
        if dup:
            self.msg.duplicate_interfaces(typ, dup)
            return 
        
        base = typ.base
        while base:
            # Avoid duplicate error messages.
            if find_duplicate(base.interfaces):
                return 
            
            ifaces.extend(base.interfaces)
            dup = find_duplicate(ifaces)
            if dup:
                self.msg.duplicate_interfaces(typ, dup)
                return 
            base = base.base
    
    def check_interface_errors(self, typ):
        interfaces = typ.all_directly_implemented_interfaces()
        for iface in interfaces:
            for n in iface.methods.keys():
                if not typ.has_method(n):
                    self.msg.interface_member_not_implemented(typ, iface, n)
    
    #
    # Statements
    #
    
    def visit_block(self, b):
        for s in b.body:
            self.accept(s)
    
    def visit_assignment_stmt(self, s):
        """Type check an assignment statement. Handle all kinds of assignment
        statements (simple, indexed, multiple).
        """
        # TODO support chained assignment x = y = z
        if len(s.lvalues) > 1:
            self.msg.not_implemented('chained assignment', s)

        self.check_assignments(self.expand_lvalues(s.lvalues[0]), s.rvalue)

    def check_assignments(self, lvalues, rvalue):        
        # Collect lvalue types. Index lvalues require special consideration,
        # since we cannot typecheck them until we know the rvalue type.
        lvalue_types = []    # May be None
        # Base type and index types (or None)
        index_lvalue_types = []
        inferred = []
        is_inferred = False
        
        for lv in lvalues:
            if self.is_definition(lv):
                is_inferred = True
                if isinstance(lv, NameExpr):
                    n = lv
                    inferred.append((n.node))
                else:
                    m = lv
                    inferred.append(m.def_var)
                lvalue_types.append(None)
                index_lvalue_types.append(None)
            elif isinstance(lv, IndexExpr):
                ilv = lv
                lvalue_types.append(None)
                index_lvalue_types.append((self.accept(ilv.base), ilv.index))
                inferred.append(None)
            else:
                lvalue_types.append(self.accept(lv))
                index_lvalue_types.append(None)
                inferred.append(None)
        
        if len(lvalues) == 1:
            # Single lvalue.
            self.check_single_assignment(lvalue_types[0],
                                         index_lvalue_types[0], rvalue, rvalue)
        else:
            self.check_multi_assignment(lvalue_types, index_lvalue_types,
                                        rvalue, rvalue)
        if is_inferred:
            self.infer_variable_type(inferred, self.accept(rvalue), rvalue)
    
    def is_definition(self, s):
        return ((isinstance(s, NameExpr) or isinstance(s, MemberExpr)) and
                s.is_def)
    
    def expand_lvalues(self, n):
        if isinstance(n, TupleExpr):
            return self.expr_checker.unwrap_list((n).items)
        elif isinstance(n, ListExpr):
            return self.expr_checker.unwrap_list((n).items)
        elif isinstance(n, ParenExpr):
            return self.expand_lvalues((n).expr)
        else:
            return [n]
    
    def infer_variable_type(self, names, init_type, context):
        """Infer the type of initialized variables from the type of the
        initializer expression.
        """
        if isinstance(init_type, Void):
            self.check_not_void(init_type, context)
        elif not self.is_valid_inferred_type(init_type):
            # We cannot use the type of the initialization expression for type
            # inference (it's not specific enough).
            self.fail(messages.NEED_ANNOTATION_FOR_VAR, context)
        else:
            # Infer type of the target.
            
            # Make the type more general (strip away function names etc.).
            init_type = self.strip_type(init_type)
            
            if len(names) > 1:
                if isinstance(init_type, TupleType):
                    tinit_type = init_type
                    # Initializer with a tuple type.
                    if len(tinit_type.items) == len(names):
                        for i in range(len(names)):
                            if names[i]:
                                names[i].typ = Annotation(tinit_type.items[i],
                                                          -1)
                    else:
                        self.msg.incompatible_value_count_in_assignment(
                            len(names), len(tinit_type.items), context)
                elif (isinstance(init_type, Instance) and
                        (init_type).typ.full_name() ==
                            'builtins.list'):
                    # Initializer with an array type.
                    item_type = (init_type).args[0]
                    for j in range(len(names)):
                        if names[j]:
                            names[j].typ = Annotation(item_type, -1)
                elif isinstance(init_type, Any):
                    for k in range(len(names)):
                        if names[k]:
                            names[k].typ = Annotation(Any(), -1)
                else:
                    self.fail(messages.INCOMPATIBLE_TYPES_IN_ASSIGNMENT,
                              context)
            else:
                for v in names:
                    v.typ = Annotation(init_type, -1)
    
    def is_valid_inferred_type(self, typ):
        """Is an inferred type invalid (e.g. the nil type or a type with a nil
        component)?
        """
        if is_same_type(typ, NoneTyp()):
            return False
        elif isinstance(typ, Instance):
            for arg in (typ).args:
                if not self.is_valid_inferred_type(arg):
                    return False
        elif isinstance(typ, TupleType):
            for item in (typ).items:
                if not self.is_valid_inferred_type(item):
                    return False
        return True
    
    def strip_type(self, typ):
        """Remove a copy of type with all 'debugging information' (e.g. name of
        function) removed.
        """
        if isinstance(typ, Callable):
            ctyp = typ
            return Callable(ctyp.arg_types,
                            ctyp.min_args,
                            ctyp.is_var_arg,
                            ctyp.ret_type,
                            ctyp.is_type_obj(),
                            None,
                            ctyp.variables)
        else:
            return typ
    
    def check_multi_assignment(self, lvalue_types, index_lvalue_types, rvalue, context, msg=None):
        if not msg:
            msg = messages.INCOMPATIBLE_TYPES_IN_ASSIGNMENT
        rvalue_type = self.accept(rvalue) # TODO maybe elsewhere; redundant
        # Try to expand rvalue to lvalue(s).
        if isinstance(rvalue_type, Any):
            pass
        elif isinstance(rvalue_type, TupleType):
            # Rvalue with tuple type.
            trvalue = rvalue_type
            items = []
            for i in range(len(lvalue_types)):
                if lvalue_types[i]:
                    items.append(lvalue_types[i])
                elif i < len(trvalue.items):
                    # TODO Figure out more precise type context, probably
                    #      based on the type signature of the _set method.
                    items.append(trvalue.items[i])
            trvalue = (self.accept(rvalue, TupleType(items)))
            if len(trvalue.items) != len(lvalue_types):
                self.msg.incompatible_value_count_in_assignment(
                    len(lvalue_types), len(trvalue.items), context)
            else:
                # The number of values is compatible. Check their types.
                for j in range(len(lvalue_types)):
                    self.check_single_assignment(
                        lvalue_types[j], index_lvalue_types[j],
                        self.temp_node(trvalue.items[j]), context, msg)
        elif (isinstance(rvalue_type, Instance) and
                (rvalue_type).typ.full_name() == 'builtins.list'):
            # Rvalue with Array type.
            item_type = (rvalue_type).args[0]
            for k in range(len(lvalue_types)):
                self.check_single_assignment(lvalue_types[k],
                                             index_lvalue_types[k],
                                             self.temp_node(item_type),
                                             context, msg)
        else:
            self.fail(msg, context)
    
    def check_single_assignment(self, lvalue_type, index_lvalue, rvalue, context, msg=messages.INCOMPATIBLE_TYPES_IN_ASSIGNMENT):
        if lvalue_type:
            rvalue_type = self.accept(rvalue, lvalue_type)      
            self.check_subtype(rvalue_type, lvalue_type, context, msg)
        elif index_lvalue:
            self.check_indexed_assignment(index_lvalue, rvalue, context)
    
    def check_indexed_assignment(self, lvalue, rvalue, context):
        """Type check indexed assignment base[index] = rvalue.

        The lvalueTypes argument is the tuple (base type, index), the
        rvaluaType is the type of the rvalue.
        """
        method_type = self.expr_checker.analyse_external_member_access(
            '__setitem__', lvalue[0], context)
        return self.expr_checker.check_call(method_type, [lvalue[1], rvalue],
                                            context)
    
    def visit_expression_stmt(self, s):
        self.accept(s.expr)
    
    def visit_return_stmt(self, s):
        """Type check a return statement."""
        if self.is_within_function():
            if s.expr:
                # Return with a value.
                typ = self.accept(s.expr, self.return_types[-1])
                # Returning a value of type dynamic is always fine.
                if not isinstance(typ, Any):
                    if isinstance(self.return_types[-1], Void):
                        self.fail(messages.NO_RETURN_VALUE_EXPECTED, s)
                    else:
                        self.check_subtype(
                            typ, self.return_types[-1], s,
                            messages.INCOMPATIBLE_RETURN_VALUE_TYPE)
            else:
                # Return without a value.
                if (not isinstance(self.return_types[-1], Void) and
                        not self.is_dynamic_function()):
                    self.fail(messages.RETURN_VALUE_EXPECTED, s)
    
    def visit_if_stmt(self, s):
        """Type check an if statement."""
        for e in s.expr:
            t = self.accept(e)
            self.check_not_void(t, e)
        for b in s.body:
            self.accept(b)
        if s.else_body:
            self.accept(s.else_body)
    
    def visit_while_stmt(self, s):
        """Type check a while statement."""
        t = self.accept(s.expr)
        self.check_not_void(t, s)
        self.accept(s.body)
        if s.else_body:
            self.accept(s.else_body)
    
    def visit_operator_assignment_stmt(self, s):
        """Type check an operator assignment statement, e.g. x += 1."""
        lvalue_type = self.accept(s.lvalue)
        rvalue_type = self.expr_checker.check_op(op_methods[s.op], lvalue_type,
                                                 s.rvalue, s)
        
        if isinstance(s.lvalue, IndexExpr):
            lv = s.lvalue
            self.check_single_assignment(None,
                                         (self.accept(lv.base), lv.index),
                                         s.rvalue, s.rvalue)
        else:
            if not is_subtype(rvalue_type, lvalue_type):
                self.msg.incompatible_operator_assignment(s.op, s)
    
    def visit_assert_stmt(self, s):
        self.accept(s.expr)
    
    def visit_raise_stmt(self, s):
        """Type check a raise statement."""
        typ = self.accept(s.expr)
        self.check_subtype(typ, self.named_type('builtins.BaseException'), s,
                           messages.INVALID_EXCEPTION_TYPE)
    
    def visit_try_stmt(self, s):
        """Type check a try statement."""
        self.accept(s.body)
        for i in range(len(s.handlers)):
            if s.types[i]:
                t = self.exception_type(s.types[i])
                if s.vars[i]:
                    s.vars[i].typ = Annotation(t)
            self.accept(s.handlers[i])
        if s.finally_body:
            self.accept(s.finally_body)
        if s.else_body:
            self.accept(s.else_body)
    
    def exception_type(self, n):
        if isinstance(n, NameExpr):
            name = n
            if isinstance(name.node, TypeInfo):
                return self.check_exception_type(name.node, n)
        elif isinstance(n, MemberExpr):
            m = n
            if isinstance(m.node, TypeInfo):
                return self.check_exception_type(m.node, n)
        elif isinstance(self.expr_checker.unwrap(n), TupleExpr):
            self.fail('Multiple exception types not supported yet', n)
            return Any()
        self.fail('Unsupported exception', n)
        return Any()

    def check_exception_type(self, info, context):
        t = Instance(info, [])
        if is_subtype(t, self.named_type('builtins.BaseException')):
            return t
        else:
            self.fail(messages.INVALID_EXCEPTION_TYPE, context)
            return Any()

    def visit_for_stmt(self, s):
        """Type check a for statement."""
        iterable = self.accept(s.expr)
        
        self.check_not_void(iterable, s.expr)
        self.check_subtype(iterable,
                           self.named_generic_type('builtins.Iterable',
                                                   [Any()]),
                           s.expr, messages.ITERABLE_EXPECTED)
        
        method = None
        
        echk = self.expr_checker
        method = echk.analyse_external_member_access('__iter__', iterable,
                                                     s.expr)
        iterator = echk.check_call(method, [], s.expr)
        method = echk.analyse_external_member_access('__next__', iterator,
                                                     s.expr)
        item = echk.check_call(method, [], s.expr)
        
        if not s.is_annotated():
            # Create a temporary copy of variables with Node item type.
            # TODO this is ugly
            index = []
            for i in s.index:
                index.append(i)
            self.check_assignments(index, self.temp_node(item, s.expr))
        elif len(s.index) == 1:
            v = s.index[0].node
            if v.typ:
                self.check_single_assignment(v.typ.typ, None,
                                           self.temp_node(item), s,
                                           messages.INCOMPATIBLE_TYPES_IN_FOR)
        else:
            t = []
            for ii in s.index:
                v = ii.node
                if v.typ:
                    t.append(v.typ.typ)
                else:
                    t.append(Any())
            self.check_multi_assignment(
                t, [None] * len(s.types),
                self.temp_node(item), s.expr,
                messages.INCOMPATIBLE_TYPES_IN_FOR)
        
        self.accept(s.body)
    
    def visit_del_stmt(self, s):
        if isinstance(s.expr, IndexExpr):
            e = s.expr  # Cast
            m = MemberExpr(e.base, '__delitem__')
            m.line = s.line
            c = CallExpr(m, [e.index])
            c.line = s.line
            return c.accept(self)
        else:
            return None # this case is handled in semantical analysis
    
    #
    # Expressions
    #
    
    def visit_name_expr(self, e):
        return self.expr_checker.visit_name_expr(e)
    
    def visit_paren_expr(self, e):
        return self.expr_checker.visit_paren_expr(e)
    
    def visit_call_expr(self, e):
        return self.expr_checker.visit_call_expr(e)
    
    def visit_member_expr(self, e):
        return self.expr_checker.visit_member_expr(e)
    
    def visit_int_expr(self, e):
        return self.expr_checker.visit_int_expr(e)
    
    def visit_str_expr(self, e):
        return self.expr_checker.visit_str_expr(e)
    
    def visit_bytes_expr(self, e):
        return self.expr_checker.visit_bytes_expr(e)
    
    def visit_float_expr(self, e):
        return self.expr_checker.visit_float_expr(e)
    
    def visit_op_expr(self, e):
        return self.expr_checker.visit_op_expr(e)
    
    def visit_unary_expr(self, e):
        return self.expr_checker.visit_unary_expr(e)
    
    def visit_index_expr(self, e):
        return self.expr_checker.visit_index_expr(e)
    
    def visit_cast_expr(self, e):
        return self.expr_checker.visit_cast_expr(e)
    
    def visit_super_expr(self, e):
        return self.expr_checker.visit_super_expr(e)
    
    def visit_type_application(self, e):
        return self.expr_checker.visit_type_application(e)
    
    def visit_list_expr(self, e):
        return self.expr_checker.visit_list_expr(e)
    
    def visit_tuple_expr(self, e):
        return self.expr_checker.visit_tuple_expr(e)
    
    def visit_dict_expr(self, e):
        return self.expr_checker.visit_dict_expr(e)
    
    def visit_slice_expr(self, e):
        return self.expr_checker.visit_slice_expr(e)
    
    def visit_func_expr(self, e):
        return self.expr_checker.visit_func_expr(e)
    
    def visit_temp_node(self, e):
        return e.typ

    #
    # Currently unsupported features
    #

    def visit_list_comprehension(self, e):
        return self.msg.not_implemented('list comprehension', e)

    def visit_set_expr(self, e):
        return self.msg.not_implemented('set literal', e)

    def visit_conditional_expr(self, e):
        return self.msg.not_implemented('conditional expression', e)

    def visit_generator_expr(self, e):
        return self.msg.not_implemented('generator expression', e)

    def visit_decorator(self, e):
        return self.msg.not_implemented('decorator', e)
    
    def visit_yield_stmt(self, s):
        self.msg.not_implemented('yield statement', s)
    
    def visit_with_stmt(self, s):
        self.msg.not_implemented('with statement', s)
    
    #
    # Helpers
    #
    
    def check_subtype(self, subtype, supertype, context, msg=messages.INCOMPATIBLE_TYPES):
        """Generate an error if the subtype is not compatible with
        supertype."""
        if not is_subtype(subtype, supertype):
            if isinstance(subtype, Void):
                self.msg.does_not_return_value(subtype, context)
            else:
                self.fail(msg, context)
    
    def named_type(self, name):
        """Return an instance type with type given by the name and no
        type arguments. For example, namedType('builtins.object')
        produces the object type.
        """
        # Assume that the name refers to a type.
        sym = self.lookup_qualified(name)
        return Instance(sym.node, [])
    
    def named_type_if_exists(self, name):
        """Return named instance type, or UnboundType if the type was
        not defined.
        
        This is used to simplify test cases by avoiding the need to
        define basic types not needed in specific test cases (tuple
        etc.).
        """
        try:
            # Assume that the name refers to a type.
            sym = self.lookup_qualified(name)
            return Instance(sym.node, [])
        except KeyError:
            return UnboundType(name)
    
    def named_generic_type(self, name, args):
        """Return an instance with the given name and type
        arguments. Assume that the number of arguments is correct.
        """
        # Assume that the name refers to a compatible generic type.
        sym = self.lookup_qualified(name)
        return Instance(sym.node, args)
    
    def type_type(self):
        """Return instance type 'type'."""
        return self.named_type('builtins.type')
    
    def object_type(self):
        """Return instance type 'object'."""
        return self.named_type('builtins.object')
    
    def bool_type(self):
        """Return instance type 'bool'."""
        return self.named_type('builtins.bool')
    
    def tuple_type(self):
        """Return instance type 'tuple'."""
        # We need the tuple for analysing member access. We want to be able to
        # do this even if tuple type is not available (useful in test cases),
        # so we return an unbound type if there is no tuple type.
        return self.named_type_if_exists('builtins.tuple')
    
    def check_type_equivalency(self, t1, t2, node, msg=messages.INCOMPATIBLE_TYPES):
        """Generate an error if the types are not equivalent. The
        dynamic type is equivalent with all types.
        """
        if not is_equivalent(t1, t2):
            self.fail(msg, node)
    
    def store_type(self, node, typ):
        """Store the type of a node in the type map."""
        self.type_map[node] = typ
    
    def is_dynamic_function(self):
        return len(self.dynamic_funcs) > 0 and self.dynamic_funcs[-1]
    
    def lookup(self, name, kind):
        """Look up a definition from the symbol table with the given name.
        TODO remove kind argument
        """
        if self.locals is not None and name in self.locals:
            return self.locals[name]
        elif self.class_tvars is not None and name in self.class_tvars:
            return self.class_tvars[name]
        elif name in self.globals:
            return self.globals[name]
        else:
            b = self.globals.get('__builtins__', None)
            if b:
                table = (b.node).names
                if name in table:
                    return table[name]
            raise KeyError('Failed lookup: {}'.format(name))
    
    def lookup_qualified(self, name):
        if '.' not in name:
            return self.lookup(name, GDEF) # FIX kind
        else:
            parts = name.split('.')
            n = self.modules[parts[0]]
            for i in range(1, len(parts) - 1):
                n = ((n.names.get(parts[i], None).node))
            return n.names[parts[-1]]
    
    def enter(self):
        self.locals = SymbolTable()
    
    def leave(self):
        self.locals = None
    
    def basic_types(self):
        """Return a BasicTypes instance that contains primitive types that are
        needed for certain type operations (joins, for example).
        """
        # TODO function type
        return BasicTypes(self.object_type(), self.type_type(),
                          self.named_type_if_exists('builtins.tuple'),
                          self.named_type_if_exists('builtins.function'))
    
    def is_within_function(self):
        """Are we currently type checking within a function (i.e. not
        at class body or at the top level)?
        """
        return self.return_types != []
    
    def check_not_void(self, typ, context):
        """Generate an error if the type is Void."""
        if isinstance(typ, Void):
            self.msg.does_not_return_value(typ, context)
    
    def temp_node(self, t, context=None):
        """Create a temporary node with the given, fixed type."""
        temp = TempNode(t)
        if context:
            temp.set_line(context.get_line())
        return temp
    
    def fail(self, msg, context):
        """Produce an error message."""
        self.msg.fail(msg, context)


def map_type_from_supertype( typ, sub_info, super_info):
    """Map type variables in a type defined in a supertype context to be valid
    in the subtype context. Assume that the result is unique; if more than
    one type is possible, return one of the alternatives.
    
    For example, assume
    
      class D<S> ...
      class C<T> is D<E<T>> ...
    
    Now S in the context of D would be mapped to E<T> in the context of C.
    """
    # Create the type of self in subtype, of form t<a1, ...>.
    inst_type = self_type(sub_info)
    # Map the type of self to supertype. This gets us a description of the
    # supertype type variables in terms of subtype variables, i.e. t<t1, ...>
    # so that any type variables in tN are to be interpreted in subtype
    # context.
    inst_type = map_instance_to_supertype(inst_type, super_info)
    # Finally expand the type variables in type with those in the previously
    # constructed type. Note that both type and instType may have type
    # variables, but in type they are interpreterd in supertype context while
    # in instType they are interpreted in subtype context. This works even if
    # the names of type variables in supertype and subtype overlap.
    return expand_type_by_instance(typ, inst_type)


def find_duplicate( list):
    """If the list has duplicates, return one of the duplicates.

    Otherwise, return None.
    """
    for i in range(1, len(list)):
        if list[i] in list[:i]:
            return list[i]
    return None
