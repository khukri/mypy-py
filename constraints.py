from mtypes import (
    Callable, Typ, TypeVisitor, UnboundType, Any, Void, NoneTyp, TypeVar,
    Instance, TupleType
)
from expandtype import expand_caller_var_args
from subtypes import map_instance_to_supertype


def infer_constraints_for_callable( callee, arg_types, is_var_arg):
    """Infer type variable constraints for a callable and a list of
    argument types.  Return a list of constraints.
    """
    # FIX check argument counts
    
    callee_num_args = callee.max_fixed_args()
    
    constraints = []
    
    caller_rest = None # Rest of types for varargs calls
    if is_var_arg:
        arg_types, caller_rest = expand_caller_var_args(arg_types,
                                                        callee_num_args)
        if arg_types is None:
            # Invalid varargs arguments.
            return []
        
        if caller_rest is not None and callee.is_var_arg:
            c = infer_constraints(callee.arg_types[-1], caller_rest,
                                  SUPERTYPE_OF)
            constraints.extend(c)
    
    caller_num_args = len(arg_types)
    
    # Infer constraints for fixed arguments.
    for i in range(min(callee_num_args, caller_num_args)):
        c = infer_constraints(callee.arg_types[i], arg_types[i],
                              SUPERTYPE_OF)
        constraints.extend(c)
    
    # Infer constraints for varargs.
    if callee.is_var_arg:
        for j in range(callee_num_args, caller_num_args):
            c = infer_constraints(callee.arg_types[-1], arg_types[j],
                                  SUPERTYPE_OF)
            constraints.extend(c)
    return constraints


def infer_constraints( template, actual, direction):
    """Infer type constraints.

    Match a template type, which may contain type variable references,
    recursively against a type which does not contain (the same) type
    variable references. The result is a list of type constrains of
    form 'T is a supertype/subtype of x', where T is a type variable
    present in the the template and x is a type without reference to
    type variables present in the template.
    
    Assume T and S are type variables. Now the following results can be
    calculated (read as '(template, actual) --> result'):
    
      (T, X)            -->  T :> X
      (X<T>, X<Y>)      -->  T <: Y and T :> Y
      ((T, T), (X, Y))  -->  T :> X and T :> Y
      ((T, S), (X, Y))  -->  T :> X and S :> Y
      (X<T>, dynamic)   -->  T <: dynamic and T :> dynamic
    
    The constraints are represented as Constraint objects.
    """
    return template.accept(ConstraintBuilderVisitor(actual, direction))


SUBTYPE_OF = 0
SUPERTYPE_OF = 1


class Constraint:
    """A representation of a type constraint, either T <: type or T :>
    type (T is a type variable).
    """
    type_var = None   # Type variable id
    op = None         # SUBTYPE_OF or SUPERTYPE_OF
    target = None
    
    def __repr__(self):
        op_str = '<:'
        if self.op == SUPERTYPE_OF:
            op_str = ':>'
        return '{} {} {}'.format(self.type_var, op_str, self.target)
    
    def __init__(self, type_var, op, target):
        self.type_var = type_var
        self.op = op
        self.target = target


class ConstraintBuilderVisitor(TypeVisitor):
    """Visitor class for inferring type constraints."""
    
    actual = None # The type that is compared against a template
    
    def __init__(self, actual, direction):
        # Direction must be SUBTYPE_OF or SUPERTYPE_OF.
        self.actual = actual
        self.direction = direction
    
    # Trivial leaf types
    
    def visit_unbound_type(self, template):
        return []
    
    def visit_any(self, template):
        return []
    
    def visit_void(self, template):
        return []
    
    def visit_none_type(self, template):
        return []
    
    # Non-trivial leaf type
    
    def visit_type_var(self, template):
        return [Constraint(template.id, SUPERTYPE_OF, self.actual)]
    
    # Non-leaf types
    
    def visit_instance(self, template):
        actual = self.actual
        if isinstance(actual, Instance):
            res = []
            instance = actual
            if (self.direction == SUBTYPE_OF and
                    template.typ.has_base(instance.typ.full_name())):
                mapped = map_instance_to_supertype(template, instance.typ)
                for i in range(len(instance.args)):
                    # The constraints for generic type parameters are
                    # invariant. Include the default constraint and its
                    # negation to achieve the effect.
                    cb = infer_constraints(mapped.args[i], instance.args[i],
                                           self.direction)
                    res.extend(cb)
                    res.extend(negate_constraints(cb))
                return res
            elif (self.direction == SUPERTYPE_OF and
                    instance.typ.has_base(template.typ.full_name())):
                mapped = map_instance_to_supertype(instance, template.typ)
                for j in range(len(template.args)):
                    # The constraints for generic type parameters are
                    # invariant.
                    cb = infer_constraints(template.args[j], mapped.args[j],
                                           self.direction)
                    res.extend(cb)
                    res.extend(negate_constraints(cb))
                return res
        if isinstance(actual, Any):
            # IDEA: Include both ways, i.e. add negation as well?
            return self.infer_against_any(template.args)
        else:
            return []
    
    def visit_callable(self, template):
        if isinstance(self.actual, Callable):
            cactual = self.actual
            # FIX verify argument counts
            # FIX what if one of the functions is generic
            res = []
            for i in range(len(template.arg_types)):
                # Negate constraints due function argument type contravariance.
                res.extend(negate_constraints(infer_constraints(
                    template.arg_types[i], cactual.arg_types[i],
                    self.direction)))
            res.extend(infer_constraints(template.ret_type, cactual.ret_type,
                                         self.direction))
            return res
        elif isinstance(self.actual, Any):
            # FIX what if generic
            res = self.infer_against_any(template.arg_types)
            res.extend(infer_constraints(template.ret_type, Any(),
                                         self.direction))
            return res
        else:
            return []
    
    def visit_tuple_type(self, template):
        actual = self.actual
        if (isinstance(actual, TupleType) and
                len((actual).items) == len(template.items)):
            res = []
            for i in range(len(template.items)):
                res.extend(infer_constraints(template.items[i],
                                             (actual).items[i],
                                             self.direction))
            return res
        elif isinstance(actual, Any):
            return self.infer_against_any(template.items)
        else:
            return []
    
    def infer_against_any(self, types):
        res = []
        for t in types:
            res.extend(infer_constraints(t, Any(), self.direction))
        return res


def negate_constraints( constraints):
    res = []
    for c in constraints:
        res.append(Constraint(c.type_var, neg_op(c.op), c.target))
    return res


def neg_op( op):
    """Map SubtypeOf to SupertypeOf and vice versa."""
    if op == SUBTYPE_OF:
        return SUPERTYPE_OF
    elif op == SUPERTYPE_OF:
        return SUBTYPE_OF
    else:
        raise ValueError('Invalid operator {}'.format(op))
