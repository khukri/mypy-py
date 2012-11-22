"""Classes for representing mypy types."""

import nodes


class Typ(nodes.Context):
    """Abstract base class for all types."""
    line = None
    repr = None
    
    def __init__(self, line=-1, repr=None):
        self.line = line
        self.repr = repr

    def get_line(self):
        return self.line
    
    def accept(self, visitor):
        raise RuntimeError('Not implemented')
    
    def __repr__(self):
        return self.accept(TypeStrVisitor())


class UnboundType(Typ):
    """Instance type that has not been bound during semantic analysis."""
    name = None
    args = None
    
    def __init__(self, name, args=None, line=-1, repr=None):
        if not args:
            args = []
        self.name = name
        self.args = args
        super().__init__(line, repr)
    
    def accept(self, visitor):
        return visitor.visit_unbound_type(self)


class ErrorType(Typ):
    """The error type is only used as a result of join and meet
    operations, when the result is undefined.
    """
    def accept(self, visitor):
        return visitor.visit_error_type(self)


class Any(Typ):
    """The type "any"."""
    def accept(self, visitor):
        return visitor.visit_any(self)


class Void(Typ):
    """The return type 'void'. This can only be used as the return type in a
    callable type and as the result type of calling such callable.
    """
    source = None   # May be None; function that generated this value
    
    def __init__(self, source=None, line=-1, repr=None):
        self.source = source
        super().__init__(line, repr)
    
    def accept(self, visitor):
        return visitor.visit_void(self)
    
    def with_source(self, source):
        return Void(source, self.line, self.repr)


class NoneTyp(Typ):
    """The type of 'None'. This is only used internally during type
    inference.  Programs cannot declare a variable of this type, and
    the type checker refuses to infer this type for a
    variable. However, subexpressions often have this type. Note that
    this is not used as the result type when calling a function with a
    void type, even though semantically such a function returns a None
    value; the void type is used instead so that we can report an
    error if the caller tries to do anything with the return value.
    """
    def __init__(self, line=-1, repr=None):
        super().__init__(line, repr)
    
    def accept(self, visitor):
        return visitor.visit_none_type(self)


class Instance(Typ):
    """An instance type of form C<T1, ..., Tn>. Type variables Tn may
    be empty"""
    typ = None
    args = None
    erased = None      # True if result of type variable substitution
    
    def __init__(self, typ, args, line=-1, repr=None, erased=False):
        self.typ = typ
        self.args = args
        self.erased = erased
        super().__init__(line, repr)
    
    def accept(self, visitor):
        return visitor.visit_instance(self)


BOUND_VAR = 2
OBJECT_VAR = 3


class TypeVar(Typ):
    """A type variable type. This refers to either a class type variable
    (id > 0) or a function type variable (id < 0).
    """
    name = None # Name of the type variable (for messages and debugging)
    id = None # 1, 2, ... for type-related, -1, ... for function-related
    
    # True if refers to the value of the type variable stored in a generic
    # instance wrapper. This is only relevant for generic class wrappers. If
    # False (default), this refers to the type variable value(s) given as the
    # implicit type variable argument.
    #
    # Can also be BoundVar/ObjectVar TODO better representation
    is_wrapper_var = None
    
    def __init__(self, name, id, is_wrapper_var=False, line=-1, repr=None):
        self.name = name
        self.id = id
        self.is_wrapper_var = is_wrapper_var
        super().__init__(line, repr)
    
    def accept(self, visitor):
        return visitor.visit_type_var(self)


class FunctionLike(Typ):
    """Abstract base class for function types (Callable and
    OverloadedCallable)."""
    def is_type_obj(self):
        pass
    
    def items(self): # Abstract
        pass
    
    def with_name(self, name): # Abstract
        pass


class Callable(FunctionLike):
    """Type of a non-overloaded callable object (function)."""
    arg_types = None # Types of function arguments
    min_args = None        # Minimum number of arguments
    is_var_arg = None     # Is it a varargs function?
    ret_type = None        # Return value type
    name = None            # Name (may be None; for error messages)
    variables = None  # Type variables for a generic function
    
    # Implicit bound values of type variables. These can be either for
    # class type variables or for generic function type variables.
    # For example, the method 'append' of Array<Int> has implicit value Int for
    # the Array type variable; the explicit method type is just
    # 'def append(Int)', without any type variable. Implicit values are needed
    # for runtime type checking, but they do not affect static type checking.
    #
    # All class type arguments must be stored first, ordered by id,
    # and function type arguments must be stored next, again ordered by id
    # (absolute value this time).
    #
    # Stored as tuples (id, type).
    bound_vars = None
    
    _is_type_obj = None # Does this represent a type object?
    
    def __init__(self, arg_types, min_args, is_var_arg, ret_type, is_type_obj, name=None, variables=None, bound_vars=None, line=-1, repr=None):
        if not variables:
            variables = TypeVars([])
        if not bound_vars:
            bound_vars = []
        self.arg_types = arg_types
        self.min_args = min_args
        self.is_var_arg = is_var_arg
        self.ret_type = ret_type
        self._is_type_obj = is_type_obj
        assert not name or '<bound method' not in name
        self.name = name
        self.variables = variables
        self.bound_vars = bound_vars
        super().__init__(line, repr)
    
    def is_type_obj(self):
        return self._is_type_obj
    
    def accept(self, visitor):
        return visitor.visit_callable(self)
    
    def with_name(self, name):
        """Return a copy of this type with the specified name."""
        ret = self.ret_type
        if isinstance(ret, Void):
            ret = (ret).with_source(name)
        return Callable(self.arg_types,
                        self.min_args,
                        self.is_var_arg,
                        ret,
                        self.is_type_obj(),
                        name,
                        self.variables,
                        self.bound_vars,
                        self.line, self.repr)
    
    def max_fixed_args(self):
        n = len(self.arg_types)
        if self.is_var_arg:
            n -= 1
        return n
    
    def items(self):
        return [self]
    
    def is_generic(self):
        return self.variables.items != []
    
    def type_var_ids(self):
        a = []
        for tv in self.variables.items:
            a.append(tv.id)
        return a


class Overloaded(FunctionLike):
    """Overloaded function type T1, ... Tn, where each Ti is Callable.
    
    The variant to call is chosen based on runtime argument types; the first
    matching signature is the target.
    """
    _items = None # Must not be empty
    
    def __init__(self, items):
        self._items = items
        super().__init__(items[0].line, None)
    
    def items(self):
        return self._items
    
    def name(self):
        return self._items[0].name
    
    def is_type_obj(self):
        # All the items must have the same type object status, so it's
        # sufficient to query only one of them.
        return self._items[0].is_type_obj()
    
    def with_name(self, name):
        ni = []
        for it in self._items:
            ni.append(it.with_name(name))
        return Overloaded(ni)
    
    def accept(self, visitor):
        return visitor.visit_overloaded(self)


class TupleType(Typ):
    """The tuple type tuple<T1, ..., Tn> (at least one type argument)."""
    items = None
    
    def __init__(self, items, line=-1, repr=None):
        self.items = items
        super().__init__(line, repr)
    
    def length(self):
        return len(self.items)
    
    def accept(self, visitor):
        return visitor.visit_tuple_type(self)


class TypeVars:
    """Representation of type variables of a function or type (i.e.
    <T1 [: B1], ..., Tn [: Bn]>).

    TODO bounds are not supported, but they may be supported in future
    """    
    items = None
    repr = None
    
    def __init__(self, items, repr=None):
        self.items = items
        self.repr = repr
    
    def __repr__(self):
        if self.items == []:
            return ''
        a = []
        for v in self.items:
            a.append(str(v))
        return '<{}>'.format(', '.join(a))


class TypeVarDef(nodes.Context):
    """Definition of a single type variable, with an optional bound
    (for bounded polymorphism).
    """
    name = None
    id = None
    bound = None  # May be None
    line = None
    repr = None
    
    def __init__(self, name, id, bound=None, line=-1, repr=None):
        self.name = name
        self.id = id
        self.bound = bound
        self.line = line
        self.repr = repr

    def get_line(self):
        return self.line
    
    def __repr__(self):
        if self.bound is None:
            return str(self.name)
        else:
            return '{} is {}'.format(self.name, self.bound)


class RuntimeTypeVar(Typ):
    """Reference to a runtime variable that represents the value of a type
    variable. The reference can must be a expression node, but only some
    node types are properly supported (NameExpr, MemberExpr and IndexExpr
    mainly).
    """
    node = None
    
    def __init__(self, node):
        self.node = node
        super().__init__(-1, None)
    
    def accept(self, visitor):
        return visitor.visit_runtime_type_var(self)


#
# Visitor-related classes
#


class TypeVisitor:
    """Visitor class for types (Typ subclasses). The parameter T is the return
    type of the visit methods.
    """
    def visit_unbound_type(self, t):
        pass
    
    def visit_error_type(self, t):
        pass
    
    def visit_any(self, t):
        pass
    
    def visit_void(self, t):
        pass
    
    def visit_none_type(self, t):
        pass
    
    def visit_type_var(self, t):
        pass
    
    def visit_instance(self, t):
        pass
    
    def visit_callable(self, t):
        pass
    
    def visit_overloaded(self, t):
        pass
    
    def visit_tuple_type(self, t):
        pass
    
    def visit_runtime_type_var(self, t):
        pass


class TypeTranslator(TypeVisitor):
    """Identity type transformation. Subclass this and override some methods to
    implement a non-trivial transformation.
    """
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
        return Instance(t.typ, self.translate_types(t.args), t.line, t.repr)
    
    def visit_type_var(self, t):
        return t
    
    def visit_callable(self, t):
        return Callable(self.translate_types(t.arg_types),
                        t.min_args,
                        t.is_var_arg,
                        t.ret_type.accept(self),
                        t.is_type_obj(),
                        t.name,
                        self.translate_variables(t.variables),
                        self.translate_bound_vars(t.bound_vars),
                        t.line, t.repr)
    
    def visit_tuple_type(self, t):
        return TupleType(self.translate_types(t.items), t.line, t.repr)
    
    def translate_types(self, types):
        a = []
        for t in types:
            a.append(t.accept(self))
        return a
    
    def translate_bound_vars(self, types):
        a = []
        for id, t in types:
            a.append((id, t.accept(self)))
        return a

    def translate_variables(self, variables):
        return variables


class TypeStrVisitor(TypeVisitor):
    """Visitor for pretty-printing types into strings. Do not preserve original
    formatting.
    
    Notes:
     - Include implicit bound type variables of callables.
     - Represent unbound types as Foo? or Foo?<...>.
     - Represent the NoneTyp type as None.
     """
    def visit_unbound_type(self, t):
        s = t.name + '?'
        if t.args != []:
            s += '<{}>'.format(self.list_str(t.args))
        return s
    
    def visit_error_type(self, t):
        return '<ERROR>'
    
    def visit_any(self, t):
        return 'any'
    
    def visit_void(self, t):
        return 'void'
    
    def visit_none_type(self, t):
        return 'None'
    
    def visit_instance(self, t):
        s = t.typ.full_name()
        if t.erased:
            s += '*'
        if t.args != []:
            s += '<{}>'.format(self.list_str(t.args))
        return s
    
    def visit_type_var(self, t):
        if t.name is None:
            # Anonymous type variable type (only numeric id).
            return '`{}'.format(t.id)
        else:
            # Named type variable type.
            s = '{}`{}'.format(t.name, t.id)
            if t.is_wrapper_var == BOUND_VAR:
                s += '!B'
            elif t.is_wrapper_var == True:
                s += '!W'
            elif t.is_wrapper_var == OBJECT_VAR:
                s += '!O'
            return s
    
    def visit_callable(self, t):
        s = self.list_str(t.arg_types[:t.min_args])
        
        opt = t.arg_types[t.min_args:]
        if t.is_var_arg:
            opt = opt[:-1]
        
        for o in opt:
            if s != '':
                s += ', '
            s += str(o) + '='
        
        if t.is_var_arg:
            if s != '':
                s += ', '
            s += '*' + str(t.arg_types[-1])
        
        s = '({})'.format(s)
        
        if not isinstance(t.ret_type, Void):
            s += ' -> {}'.format(t.ret_type)
        
        if t.variables.items != []:
            s = '{} {}'.format(t.variables, s)
        
        if t.bound_vars != []:
            # Include implicit bound type variables.
            a = []
            for i, bt in t.bound_vars:
                a.append('{}:{}'.format(i, bt))
            s = '[{}] {}'.format(', '.join(a), s)
        
        return 'def {}'.format(s)
    
    def visit_overloaded(self, t):
        a = []
        for i in t.items():
            a.append(i.accept(self))
        return 'Overload({})'.format(', '.join(a))
    
    def visit_tuple_type(self, t):
        s = self.list_str(t.items)
        return 'tuple<{}>'.format(s)
    
    def visit_runtime_type_var(self, t):
        return '<RuntimeTypeVar>'
    
    def list_str(self, a):
        """Convert items of an array to strings (pretty-print types)
        and join the results with commas.
        """
        res = []
        for t in a:
            if isinstance(t, Typ):
                res.append(t.accept(self))
            else:
                res.append(str(t))
        return ', '.join(res)
