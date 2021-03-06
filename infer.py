import checker
from constraints import infer_constraints, infer_constraints_for_callable
from mtypes import Typ, Callable
from solve import solve_constraints
from constraints import SUBTYPE_OF


def infer_function_type_arguments( callee_type, arg_types, arg_kinds, formal_to_actual, basic):
    """Infer the type arguments of a generic function.

    Return an array of lower bound types for the type variables -1 (at
    index 0), -2 (at index 1), etc. A lower bound is None if a value
    could not be inferred.

    Arguments:
      callee_type: the target generic function
      arg_types: argument types at the call site
      arg_kinds: nodes.ARG_* values for arg_types
      formal_to_actual: mapping from formal to actual variable indices
      basic: references to basic types which are needed during inference
    """
    # Infer constraints.
    constraints = infer_constraints_for_callable(
        callee_type, arg_types, arg_kinds, formal_to_actual)
    
    # Solve constraints.
    type_vars = callee_type.type_var_ids()
    return solve_constraints(type_vars, constraints, basic)


def infer_type_arguments( type_var_ids, template, actual, basic):
    # Like infer_function_type_arguments, but only match a single type
    # against a generic type.
    constraints = infer_constraints(template, actual, SUBTYPE_OF)
    return solve_constraints(type_var_ids, constraints, basic)
