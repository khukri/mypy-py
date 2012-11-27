"""The semantic analyzer binds names to definitions and does various other
simple consistency checks. Semantic analysis is first analysis pass after
parsing."""

from nodes import (
    MypyFile, TypeInfo, Node, AssignmentStmt, FuncDef, OverloadedFuncDef,
    TypeDef, VarDef, Var, GDEF, MODULE_REF, Annotation, FuncItem, Import,
    ImportFrom, ImportAll, Block, LDEF, NameExpr, MemberExpr,
    IndexExpr, ParenExpr, TupleExpr, ListExpr, ExpressionStmt, ReturnStmt,
    RaiseStmt, YieldStmt, AssertStmt, OperatorAssignmentStmt, WhileStmt,
    ForStmt, BreakStmt, ContinueStmt, IfStmt, TryStmt, WithStmt, DelStmt,
    GlobalDecl, SuperExpr, DictExpr, CallExpr, RefExpr, OpExpr, UnaryExpr,
    SliceExpr, CastExpr, TypeApplication, Context, SymbolTable,
    SymbolTableNode, TVAR
)
from visitor import NodeVisitor
from errors import Errors
from mtypes import (
    NoneTyp, Callable, Overloaded, Instance, Typ, TypeVar, Any
)
from nodes import function_type
from typeanal import TypeAnalyser


class SemanticAnal(NodeVisitor):
    """Semantically analyze parsed mypy files.

    The analyzer binds names and does various consistency checks for a
    parse tree. Note that type checking is performed as a separate
    pass.
    """
    # Library search paths
    lib_path = None
    # Module name space
    modules = None
    # Global name space for current module
    globals = None
    # Names declared using "global" (separate set for each scope)
    global_decls = None
    # Module-local name space for current modules
    # TODO not needed?
    module_names = None
    # Class type variables (the scope is a single class definition)
    class_tvars = None
    # Local names
    locals = None
    # All classes, from name to info (TODO needed?)
    types = None
    
    stack = None         # Function local/type variable stack
    typ = None        # TypeInfo of enclosing class (or None)
    is_init_method = None # Are we now analysing __init__?
    is_function = None    # Are we now analysing a function/method?
    block_depth = None     # Depth of nested blocks
    loop_depth = None      # Depth of breakable loops
    cur_mod_id = None      # Current module id (or None) (phase 2)
    imports = None    # Imported modules (during phase 2 analysis)
    errors = None       # Keep track of generated errors
    
    def __init__(self, lib_path, errors):
        """Create semantic analyzer. Use libPath to search for
        modules, and report compile errors using the Errors instance.
        """
        self.stack = [None]
        self.imports = set()
        self.typ = None
        self.block_depth = 0
        self.loop_depth = 0
        self.types = TypeInfoMap()
        self.lib_path = lib_path
        self.errors = errors
        self.modules = {}
        self.class_tvars = None
        self.is_init_method = False
        self.is_function = False
    
    #
    # First pass of semantic analysis
    #
    
    def anal_defs(self, defs, fnam, mod_id):
        """Perform the first analysis pass.

        Resolve the full names of definitions and construct type info
        structures, but do not resolve inter-definition references
        such as base classes.
        """
        self.cur_mod_id = mod_id
        self.errors.set_file(fnam)
        self.globals = SymbolTable()
        self.global_decls = [set()]
        
        # Add implicit definition of '__name__'.
        name_def = VarDef([(Var('__name__'), Any())], True)
        defs.insert(0, name_def)
        
        for d in defs:
            if isinstance(d, AssignmentStmt):
                self.anal_assignment_stmt(d)
            elif isinstance(d, FuncDef):
                self.anal_func_def(d)
            elif isinstance(d, OverloadedFuncDef):
                self.anal_overloaded_func_def(d)
            elif isinstance(d, TypeDef):
                self.anal_type_def(d)
            elif isinstance(d, VarDef):
                self.anal_var_def(d)
            elif isinstance(d, ForStmt):
                self.anal_for_stmt(d)
        # Add implicit definition of 'None' to builtins, as we cannot define a
        # variable with a None type explicitly.
        if mod_id == 'builtins':
            none_def = VarDef([(Var('None'), NoneTyp())], True)
            defs.append(none_def)
            self.anal_var_def(none_def)
    
    def anal_assignment_stmt(self, s):
        for lval in s.lvalues:
            self.analyse_lvalue(lval, False, True)
    
    def anal_func_def(self, d):
        self.check_no_global(d.name(), d, True)
        d._full_name = self.qualified_name(d.name())
        self.globals[d.name()] = SymbolTableNode(GDEF, d, self.cur_mod_id)
    
    def anal_overloaded_func_def(self, d):
        self.check_no_global(d.name(), d)
        d._full_name = self.qualified_name(d.name())
        self.globals[d.name()] = SymbolTableNode(GDEF, d, self.cur_mod_id)
    
    def anal_type_def(self, d):
        self.check_no_global(d.name, d)
        d.full_name = self.qualified_name(d.name)
        info = TypeInfo({}, {}, d)
        info.set_line(d.line)
        self.types[d.full_name] = info
        d.info = info
        self.globals[d.name] = SymbolTableNode(GDEF, info, self.cur_mod_id)
    
    def anal_var_def(self, d):
        for v, t in d.items:
            self.check_no_global(v.name(), d)
            v._full_name = self.qualified_name(v.name())
            self.globals[v.name()] = SymbolTableNode(GDEF, v, self.cur_mod_id)

    def anal_for_stmt(self, s):
        for n in s.index:
            self.analyse_lvalue(n, False, True)
    
    #
    # Second pass of semantic analysis
    #
    
    # Do the bulk of semantic analysis in this second and final semantic
    # analysis pass (other than type checking).
    
    def visit_file(self, file_node, fnam):
        self.errors.set_file(fnam)
        self.globals = file_node.names
        self.module_names = SymbolTable()
        self.cur_mod_id = file_node.full_name()
        
        if 'builtins' in self.modules:
            self.globals['__builtins__'] = SymbolTableNode(
                MODULE_REF, self.modules['builtins'], self.cur_mod_id)
        
        defs = file_node.defs
        for d in defs:
            d.accept(self)
    
    def visit_func_def(self, defn):
        if self.locals is not None:
            self.fail('Nested functions not supported yet', defn)
            return
        if self.typ:
            defn.info = self.typ
            if not defn.is_overload:
                if defn.name() in self.typ.methods:
                    self.name_already_defined(defn.name(), defn)
                self.typ.methods[defn.name()] = defn
            if defn.name() == '__init__':
                self.is_init_method = True
            if defn.args == []:
                self.fail('Method must have at least one argument', defn)
        
        self.errors.set_function(defn.name())
        self.analyse_function(defn)
        self.errors.set_function(None)
        self.is_init_method = False
    
    def visit_overloaded_func_def(self, defn):
        t = []
        for f in defn.items:
            f.is_overload = True
            f.accept(self)
            t.append(function_type(f))
        defn.typ = Annotation(Overloaded(t))
        defn.typ.set_line(defn.line)
        
        if self.typ:
            self.typ.methods[defn.name()] = defn
            defn.info = self.typ
    
    def analyse_function(self, defn):
        self.enter()
        self.add_func_type_variables_to_symbol_table(defn)
        if defn.typ:
            defn.typ.accept(self)
            if isinstance(defn, FuncDef):
                fdef = defn
                if self.typ:
                    defn.typ.typ = (defn.typ.typ).with_name(
                        '"{}" of "{}"'.format(fdef.name(), self.typ.name()))
                else:
                    defn.typ.typ = (defn.typ.typ).with_name(
                        '"{}"'.format(fdef.name()))
                if self.typ and (defn.typ.typ).arg_types != []:
                    (defn.typ.typ).arg_types[0] = self_type(
                        fdef.info)
        for init in defn.init:
            if init:
                init.rvalue.accept(self)
        for v in defn.args:
            self.add_local(v, defn)
        if defn.var_arg:
            self.add_local(defn.var_arg, defn)
        for init_ in defn.init:
            if init_:
                init_.lvalues[0].accept(self)
        
        # The first argument of a method is self.
        if self.typ and defn.args:
            defn.args[0].is_self = True
        
        defn.body.accept(self)
        self.leave()
    
    def add_func_type_variables_to_symbol_table(self, defn):
        if defn.typ:
            tt = defn.typ.typ
            names = self.type_var_names()
            items = (tt).variables.items
            for i in range(len(items)):
                name = items[i].name
                if name in names:
                    self.name_already_defined(name, defn)
                self.add_type_var(self.locals, name, -i - 1)
                names.add(name)
    
    def type_var_names(self):
        if not self.typ:
            return set()
        else:
            return set(self.typ.type_vars)
    
    def add_type_var(self, scope, name, id):
        scope[name] = SymbolTableNode(TVAR, None, None, None, id)
    
    def visit_type_def(self, defn):
        if self.locals is not None or self.typ:
            self.fail('Nested classes not supported yet', defn)
            return
        self.typ = defn.info
        self.add_class_type_variables_to_symbol_table(self.typ)
        has_base_class = False
        for i in range(len(defn.base_types)):
            defn.base_types[i] = self.anal_type(defn.base_types[i])
            self.typ.bases.append(defn.base_types[i])
            has_base_class = has_base_class or self.is_instance_type(
                                                        defn.base_types[i])
        # Add 'object' as implicit base if there is no other base class.
        if (not defn.is_interface and not has_base_class and
                defn.full_name != 'builtins.object'):
            defn.base_types.insert(0, self.object_type())
        if defn.base_types != []:
            bt = defn.base_types
            if isinstance(bt[0], Instance):
                defn.info.base = (bt[0]).typ
            for t in bt[1:]:
                if isinstance(t, Instance):
                    defn.info.add_interface((t).typ)
        defn.defs.accept(self)
        self.class_tvars = None
        self.typ = None
    
    def object_type(self):
        sym = self.lookup_qualified('__builtins__.object', None)
        return Instance(sym.node, [])
    
    def is_instance_type(self, t):
        return isinstance(t, Instance) and not (t).typ.is_interface
    
    def add_class_type_variables_to_symbol_table(self, info):
        vars = info.type_vars
        if vars != []:
            self.class_tvars = SymbolTable()
            for i in range(len(vars)):
                self.add_type_var(self.class_tvars, vars[i], i + 1)
    
    def visit_annotation(self, ann):
        ann.typ = self.anal_type(ann.typ)
    
    def visit_import(self, i):
        if not self.check_import_at_toplevel(i):
            return
        for id, as_id in i.ids:
            if as_id != id:
                m = self.modules[id]
                self.globals[as_id] = SymbolTableNode(MODULE_REF, m,
                                                      self.cur_mod_id)
            else:
                base = id.split('.')[0]
                m = self.modules[base]
                self.globals[base] = SymbolTableNode(MODULE_REF, m,
                                                     self.cur_mod_id)
    
    def visit_import_from(self, i):
        if not self.check_import_at_toplevel(i):
            return
        m = self.modules[i.id]
        for id, as_id in i.names:
            node = m.names.get(id, None)
            if node:
                self.globals[as_id] = SymbolTableNode(node.kind, node.node,
                                                      self.cur_mod_id)
            else:
                self.fail("Module has no attribute '{}'".format(id), i)
    
    def visit_import_all(self, i):
        if not self.check_import_at_toplevel(i):
            return
        m = self.modules[i.id]
        for name, node in m.names.items():
            if not name.startswith('_'):
                self.globals[name] = SymbolTableNode(node.kind, node.node,
                                                     self.cur_mod_id)

    def check_import_at_toplevel(self, c):
        if self.block_depth > 0:
            self.fail("Imports within blocks not supported yet", c)
            return False
        else:
            return True
    
    #
    # Statements
    #
    
    def visit_block(self, b):
        self.block_depth += 1
        for s in b.body:
            s.accept(self)
        self.block_depth -= 1
    
    def visit_block_maybe(self, b):
        if b:
            self.visit_block(b)
    
    def visit_var_def(self, defn):
        for i in range(len(defn.items)):
            defn.items[i] = (defn.items[i][0],
                             self.anal_type(defn.items[i][1]))
            if defn.items[i][1]:
                defn.items[i][0].typ = Annotation(defn.items[i][1])
        
        for v, t in defn.items:
            if self.locals is not None:
                defn.kind = LDEF
                self.add_local(v, defn)
            elif self.typ:
                v.info = self.typ
                self.typ.vars[v.name()] = v
            elif v.name not in self.globals:
                defn.kind = GDEF
                self.add_var(v, defn)
        
        if defn.init:
            defn.init.accept(self)
    
    def anal_type(self, t):
        if t:
            a = TypeAnalyser(self.lookup_qualified, self.fail)
            return t.accept(a)
        else:
            return None
    
    def visit_assignment_stmt(self, s):
        for lval in s.lvalues:
            self.analyse_lvalue(lval)
        s.rvalue.accept(self)
    
    def analyse_lvalue(self, lval, nested=False, add_defs=False):
        if isinstance(lval, NameExpr):
            n = lval
            nested_global = (self.locals is None and self.block_depth > 0 and
                             not self.typ)
            if (add_defs or nested_global) and n.name not in self.globals:
                # Define new global name.
                v = Var(n.name)
                v._full_name = self.qualified_name(n.name)
                n.node = v
                n.is_def = True
                self.globals[n.name] = SymbolTableNode(GDEF, v,
                                                       self.cur_mod_id)
            elif isinstance(n.node, Var) and n.is_def:
                v = n.node
                self.module_names[v.name()] = SymbolTableNode(GDEF, v,
                                                              self.cur_mod_id)
            elif (self.locals is not None and n.name not in self.locals and
                  n.name not in self.global_decls[-1]):
                # Define new local name.
                v = Var(n.name)
                n.node = v
                n.is_def = True
                n.kind = LDEF
                self.add_local(v, n)
            elif self.locals is None and (self.typ and
                                          n.name not in self.typ.vars):
                # Define a new attribute.
                v = Var(n.name)
                v.info = self.typ
                n.node = v
                n.is_def = True
                self.typ.vars[n.name] = v
            else:
                # Bind to an existing name.
                lval.accept(self)
        elif isinstance(lval, MemberExpr):
            if not add_defs:
                self.analyse_member_lvalue(lval)
        elif isinstance(lval, IndexExpr):
            if not add_defs:
                lval.accept(self)
        elif isinstance(lval, ParenExpr):
            self.analyse_lvalue((lval).expr, nested, add_defs)
        elif (isinstance(lval, TupleExpr) or
              isinstance(lval, ListExpr)) and not nested:
            items = (lval).items
            for i in items:
                self.analyse_lvalue(i, True, add_defs)
        else:
            self.fail('Invalid assignment target', lval)
    
    def analyse_member_lvalue(self, lval):
        lval.accept(self)
        if self.is_init_method and isinstance(lval.expr, NameExpr):
            node = (lval.expr).node
            if (isinstance(node, Var) and (node).is_self and
                    lval.name not in self.typ.vars):
                lval.is_def = True
                v = Var(lval.name)
                v.info = self.typ
                lval.def_var = v
                self.typ.vars[lval.name] = v
    
    def visit_expression_stmt(self, s):
        s.expr.accept(self)
    
    def visit_return_stmt(self, s):
        if self.locals is None:
            self.fail("'return' outside function", s)
        if s.expr:
            s.expr.accept(self)
    
    def visit_raise_stmt(self, s):
        if s.expr:
            s.expr.accept(self)
    
    def visit_yield_stmt(self, s):
        if self.locals is None:
            self.fail("'yield' outside function", s)
        if s.expr:
            s.expr.accept(self)
    
    def visit_assert_stmt(self, s):
        if s.expr:
            s.expr.accept(self)
    
    def visit_operator_assignment_stmt(self, s):
        s.lvalue.accept(self)
        s.rvalue.accept(self)
    
    def visit_while_stmt(self, s):
        s.expr.accept(self)
        self.loop_depth += 1
        s.body.accept(self)
        self.loop_depth -= 1
        self.visit_block_maybe(s.else_body)
    
    def visit_for_stmt(self, s):
        s.expr.accept(self)
        
        # Bind index variables and check if they define new names.
        for n in s.index:
            self.analyse_lvalue(n)
        
        # Analyze index variable types.
        for i in range(len(s.types)):
            t = s.types[i]
            if t:
                t.accept(self)
                v = s.index[i].node
                # TODO check if redefinition
                v.typ = t
        
        # Report error if only some of the loop variables have annotations.
        if s.types != [None] * len(s.types) and None in s.types:
            self.fail('Cannot mix unannotated and annotated loop variables', s)
            
        self.loop_depth += 1
        self.visit_block(s.body)
        self.loop_depth -= 1
        
        self.visit_block_maybe(s.else_body)
    
    def visit_break_stmt(self, s):
        if self.loop_depth == 0:
            self.fail("'break' outside loop", s)
    
    def visit_continue_stmt(self, s):
        if self.loop_depth == 0:
            self.fail("'continue' outside loop", s)
    
    def visit_if_stmt(self, s):
        for i in range(len(s.expr)):
            s.expr[i].accept(self)
            self.visit_block(s.body[i])
        self.visit_block_maybe(s.else_body)
    
    def visit_try_stmt(self, s):
        s.body.accept(self)
        for i in range(len(s.types)):
            if s.types[i]:
                s.types[i].accept(self)
            if s.vars[i]:
                self.add_var(s.vars[i], s.vars[i])
            s.handlers[i].accept(self)
        self.visit_block_maybe(s.else_body)
        self.visit_block_maybe(s.finally_body)
    
    def visit_with_stmt(self, s):
        for e in s.expr:
            e.accept(self)
        for n in s.name:
            if n:
                self.add_var(n, s)
        self.visit_block(s.body)
    
    def visit_del_stmt(self, s):
        s.expr.accept(self)
        if not isinstance(s.expr, IndexExpr):
            self.fail('Invalid delete target', s)
    
    def visit_global_decl(self, g):
        for n in g.names:
            self.global_decls[-1].add(n)
    
    #
    # Expressions
    #
    
    def visit_name_expr(self, expr):
        n = self.lookup(expr.name, expr)
        if n:
            if n.kind == TVAR:
                self.fail("'{}' is a type variable and only valid in type "
                          "context".format(expr.name), expr)
            else:
                expr.kind = n.kind
                expr.node = (n.node)
                expr.full_name = n.full_name()
    
    def visit_super_expr(self, expr):
        if not self.typ:
            self.fail('"super" used outside class', expr)
            return 
        expr.info = self.typ
    
    def visit_tuple_expr(self, expr):
        for item in expr.items:
            item.accept(self)
        if expr.types:
            for i in range(len(expr.types)):
                expr.types[i] = self.anal_type(expr.types[i])
    
    def visit_list_expr(self, expr):
        for item in expr.items:
            item.accept(self)
        expr.typ = self.anal_type(expr.typ)
    
    def visit_dict_expr(self, expr):
        for key, value in expr.items:
            key.accept(self)
            value.accept(self)
        expr.key_type = self.anal_type(expr.key_type)
        expr.value_type = self.anal_type(expr.value_type)
    
    def visit_paren_expr(self, expr):
        expr.expr.accept(self)
    
    def visit_call_expr(self, expr):
        expr.callee.accept(self)
        for a in expr.args:
            a.accept(self)
        for n, v in expr.keyword_args:
            v.accept(self)
    
    def visit_member_expr(self, expr):
        base = expr.expr
        base.accept(self)
        # Bind references to module attributes.
        if isinstance(base, RefExpr) and (base).kind == MODULE_REF:
            names = ((base).node).names
            n = names.get(expr.name, None)
            if n:
                expr.kind = n.kind
                expr.full_name = n.full_name()
                expr.node = n.node
            else:
                self.fail("Module has no attribute '{}'".format(expr.name),
                          expr)
    
    def visit_op_expr(self, expr):
        expr.left.accept(self)
        expr.right.accept(self)
    
    def visit_unary_expr(self, expr):
        expr.expr.accept(self)
    
    def visit_index_expr(self, expr):
        expr.base.accept(self)
        expr.index.accept(self)
    
    def visit_slice_expr(self, expr):
        if expr.begin_index:
            expr.begin_index.accept(self)
        if expr.end_index:
            expr.end_index.accept(self)
        if expr.stride:
            expr.stride.accept(self)
    
    def visit_cast_expr(self, expr):
        expr.expr.accept(self)
        expr.typ = self.anal_type(expr.typ)
    
    def visit_type_application(self, expr):
        expr.expr.accept(self)
        for i in range(len(expr.types)):
            expr.types[i] = self.anal_type(expr.types[i])
    
    #
    # Helpers
    #
    
    def lookup(self, name, ctx):
        if name in self.global_decls[-1]:
            if name in self.globals:
                return self.globals[name]
            else:
                self.name_not_defined(name, ctx)
        elif self.locals is not None and name in self.locals:
            return self.locals[name]
        elif self.class_tvars and name in self.class_tvars:
            return self.class_tvars[name]
        elif name in self.globals:
            return self.globals[name]
        else:
            b = self.globals.get('__builtins__', None)
            if b:
                table = (b.node).names
                if name in table:
                    return table[name]
            self.name_not_defined(name, ctx)
    
    def lookup_qualified(self, name, ctx):
        if '.' not in name:
            return self.lookup(name, ctx)
        else:
            parts = name.split('.')
            n = self.lookup(parts[0], ctx)
            if n:
                for i in range(1, len(parts)):
                    n = (n.node).names.get(parts[i], None)
                    if not n:
                        self.name_not_defined(name, ctx)
            return n
    
    def qualified_name(self, n):
        return self.cur_mod_id + '.' + n
    
    def enter(self):
        self.locals = SymbolTable()
        self.global_decls.append(set())
    
    def leave(self):
        self.locals = None
        self.global_decls.pop()
    
    def add_var(self, v, ctx):
        if self.locals is not None:
            self.add_local(v, ctx)
        else:
            self.globals[v.name()] = SymbolTableNode(GDEF, v, self.cur_mod_id)
            v._full_name = self.qualified_name(v.name())
    
    def add_local(self, v, ctx):
        if v.name() in self.locals:
            self.name_already_defined(v.name(), ctx)
        v._full_name = v.name()
        self.locals[v.name()] = SymbolTableNode(LDEF, v)
    
    def check_no_global(self, n, ctx, is_func=False):
        if n in self.globals:
            if is_func and isinstance(self.globals[n].node, FuncDef):
                self.fail(("Name '{}' already defined (overload variants "
                           "must be next to each other)").format(n), ctx)
            else:
                self.name_already_defined(n, ctx)
    
    def name_not_defined(self, name, ctx):
        self.fail("Name '{}' is not defined".format(name), ctx)
    
    def name_already_defined(self, name, ctx):
        self.fail("Name '{}' already defined".format(name), ctx)
    
    def fail(self, msg, ctx):
        self.errors.report(ctx.get_line(), msg)


def self_type( typ):
    """For a non-generic type, return instance type representing the type.
    For a generic G type with parameters T1, .., Tn, return G<T1, ..., Tn>.
    """
    tv = []
    for i in range(len(typ.type_vars)):
        tv.append(TypeVar(typ.type_vars[i], i + 1))
    return Instance(typ, tv)


class TypeInfoMap(dict):
    def __str__(self):
        a = ['TypeInfoMap(']
        for x, y in sorted(self.items()):
            if isinstance(x, str) and not x.startswith('builtins.'):
                ti = ('\n' + '  ').join(str(y).split('\n'))
                a.append('  {} : {}'.format(x, ti))
        a[-1] += ')'
        return '\n'.join(a)
