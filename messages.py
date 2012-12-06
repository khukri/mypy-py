"""Facilities and constants for generating error messages during type checking.

The type checker itself does not deal with message string literals to
improve code clarity and to simplify localization (in the future)."""

import re

from errors import Errors
from mtypes import (
    Typ, Callable, Instance, TypeVar, TupleType, Void, NoneTyp, Any, Overloaded
)
from nodes import TypeInfo, Context
import checker


# Constants that represent simple type checker error message, i.e. messages
# that do not have any parameters.

NO_RETURN_VALUE_EXPECTED = 'No return value expected'
INCOMPATIBLE_RETURN_VALUE_TYPE = 'Incompatible return value type'
RETURN_VALUE_EXPECTED = 'Return value expected'
BOOLEAN_VALUE_EXPECTED = 'Boolean value expected'
BOOLEAN_EXPECTED_FOR_IF = 'Boolean value expected for if condition'
BOOLEAN_EXPECTED_FOR_WHILE = 'Boolean value expected for while condition'
BOOLEAN_EXPECTED_FOR_UNTIL = 'Boolean value expected for until condition'
BOOLEAN_EXPECTED_FOR_NOT = 'Boolean value expected for not operand'
INVALID_EXCEPTION_TYPE = 'Invalid exception type'
INCOMPATIBLE_TYPES = 'Incompatible types'
INCOMPATIBLE_TYPES_IN_ASSIGNMENT = 'Incompatible types in assignment'
INIT_MUST_NOT_HAVE_RETURN_TYPE = 'Cannot define return type for "__init__"'
GETTER_TYPE_INCOMPATIBLE_WITH_SETTER = \
                                     'Type of getter incompatible with setter'
TUPLE_INDEX_MUST_BE_AN_INT_LITERAL = 'Tuple index must an integer literal'
TUPLE_INDEX_OUT_OF_RANGE = 'Tuple index out of range'
TYPE_CONSTANT_EXPECTED = 'Type "Constant" or initializer expected'
INCOMPATIBLE_PAIR_ITEM_TYPE = 'Incompatible Pair item type'
INVALID_TYPE_APPLICATION_TARGET_TYPE = 'Invalid type application target type'
INCOMPATIBLE_TUPLE_ITEM_TYPE = 'Incompatible tuple item type'
INCOMPATIBLE_KEY_TYPE = 'Incompatible dictionary key type'
INCOMPATIBLE_VALUE_TYPE = 'Incompatible dictionary value type'
NEED_ANNOTATION_FOR_VAR = 'Need type annotation for variable'
ITERABLE_EXPECTED = 'Iterable expected'
INCOMPATIBLE_TYPES_IN_FOR = 'Incompatible types in for statement'
INCOMPATIBLE_ARRAY_VAR_ARGS = 'Incompatible variable arguments in call'
INVALID_SLICE_INDEX = 'Slice index must be an integer or None'  


class MessageBuilder:
    """Helper class for reporting type checker error messages with parameters.
    The methods of this class need to be provided with the context within a
    file; the errors member manages the wider context.
    
    IDEA: Support a "verbose mode" that includes full information about types
          in error messages and that may otherwise produce more detailed error
          messages.
          """
    # Report errors using this instance. It knows about the current file and
    # import context.
    errors = None
    
    def __init__(self, errors):
        self.errors = errors

    #
    # Helpers
    #
    
    def fail(self, msg, context):
        """Report an error message."""
        self.errors.report(context.get_line(), msg.strip())
    
    def format(self, typ):
        """Convert a type to a relatively short string that is
        suitable for error messages. Mostly behave like format_simple
        below, but never return an empty string.
        """
        s = self.format_simple(typ)
        if s != '':
            # If format_simple returns a non-trivial result, use that.
            return s
        elif isinstance(typ, Callable):
            # Use a simple representation for function types; proper function
            # types may result in long and difficult-to-read error messages.
            return 'function'
        else:
            # Default case; we simply have to return something meaningful here.
            return 'object'
    
    def format_simple(self, typ):
        """Convert simple types to string that is suitable for error messages,
        otherwise return "". Try to keep the length of the result relatively
        short to avoid overly long error messages.
        
        Examples:
          builtins.int -> 'int'
          any -> 'any'
          void -> void
          function type -> "" (empty string)
          """
        if isinstance(typ, Instance):
            itype = typ
            # Get the short name of the type.
            base_str = itype.typ.name()
            if itype.args == []:
                # No type arguments. Place the type name in quotes to avoid
                # potential for confusion: otherwise, the type name could be
                # interpreted as a normal word.
                return '"{}"'.format(base_str)
            elif itype.typ.full_name() == 'builtins.list':
                return '{}[]'.format(strip_quotes(self.format(itype.args[0])))
            else:
                # There are type arguments. Convert the arguments to strings
                # (using format() instead of format_simple() to avoid empty
                # strings). If the result is too long, replace arguments
                # with <...>.
                a = []
                for arg in itype.args:
                    a.append(strip_quotes(self.format(arg)))
                s = ', '.join(a)
                if len((base_str + s)) < 25:
                    return '{}<{}>'.format(base_str, s)
                else:
                    return '{}<...>'.format(base_str)
        elif isinstance(typ, TypeVar):
            # This is similar to non-generic instance types.
            return '"{}"'.format((typ).name)
        elif isinstance(typ, TupleType):
            items = []
            for t in (typ).items:
                items.append(strip_quotes(self.format(t)))
            s = '"tuple<{}>"'.format(', '.join(items))
            if len(s) < 30:
                return s
            else:
                return 'tuple'
        elif isinstance(typ, Void):
            return 'void'
        elif isinstance(typ, NoneTyp):
            return 'None'
        elif isinstance(typ, Any):
            return '"any"'
        elif typ is None:
            raise RuntimeError('Type is nil')
        else:
            # No simple representation for this type that would convey very
            # useful information. No need to mention the type explicitly in a
            # message.
            return ''

    #
    # Specific operations
    #
    
    # The following operations are for genering specific error messages. They
    # get some information as arguments, and they build an error message based
    # on them.
    
    def has_no_member(self, typ, member, context):
        """Report a missing or non-accessible member.  The type
        argument is the base type. If member corresponds to an
        operator, use the corresponding operator name in the
        messages. Return type Any.
        """
        if (isinstance(typ, Instance) and
                (typ).typ.has_readable_member(member)):
            self.fail('Member "{}" is not assignable'.format(member), context)
        elif isinstance(typ, Void):
            self.check_void(typ, context)
        elif member == '__contains__':
            self.fail('Unsupported right operand type for in ({})'.format(
                self.format(typ)), context)
        elif member in checker.op_methods.values():
            # Access to a binary operator member (e.g. _add). This case does
            # not handle indexing operations.
            for op, method in checker.op_methods.items():
                if method == member:
                    self.unsupported_left_operand(op, typ, context)
                    break
        elif member == '__neg__':
            self.fail('Unsupported operand type for unary - ({})'.format(
                self.format(typ)), context)
        elif member == '__invert__':
            self.fail('Unsupported operand type for ~ ({})'.format(
                self.format(typ)), context)
        elif member == '__getitem__':
            # Indexed get.
            self.fail('Value of type {} is not indexable'.format(
                self.format(typ)), context)
        elif member == '__setitem__':
            # Indexed set.
            self.fail('Unsupported target for indexed assignment', context)
        else:
            # The non-special case: a missing ordinary member.
            self.fail('{} has no member "{}"'.format(self.format(typ),
                                                     member), context)
        return Any()
    
    def unsupported_operand_types(self, op, left_type, right_type, context):
        """Report unsupported operand types for a binary operation.
        Types can be Typ objects or strings.
        """
        if isinstance(left_type, Void) or isinstance(right_type, Void):
            self.check_void(left_type, context)
            self.check_void(right_type, context)
            return 
        
        left_str = None
        if isinstance(left_type, str):
            left_str = left_type
        else:
            left_str = self.format(left_type)
        
        right_str = None
        if isinstance(right_type, str):
            right_str = right_type
        else:
            right_str = self.format(right_type)
        
        msg = 'Unsupported operand types for {} ({} and {})'.format(
                                                    op, left_str, right_str)
        self.fail(msg, context)
    
    def unsupported_left_operand(self, op, typ, context):
        if not self.check_void(typ, context):
            self.fail('Unsupported left operand type for {} ({})'.format(
                op, self.format(typ)), context)
    
    def type_expected_as_right_operand_of_is(self, context):
        self.fail('Type expected as right operand of "is"', context)
    
    def not_callable(self, typ, context):
        self.fail('{} not callable'.format(self.format(typ)), context)
        return Any()
    
    def incompatible_argument(self, n, callee, arg_type, context):
        """Report an error about an incompatible type arg_type for
        argument n when calling a value with type callee. If the
        callee represents a method that corresponds to an operator,
        use the corresponding operator name in the messages.
        """
        target = ''
        if callee.name:
            name = callee.name
            base = extract_type(name)
            
            for op, method in checker.op_methods.items():
                if name.startswith('"{}" of'.format(method)):
                    if op == 'in':
                        self.unsupported_operand_types(op, arg_type, base,
                                                       context)
                    else:
                        self.unsupported_operand_types(op, base, arg_type,
                                                       context)
                    return 
            
            if name.startswith('"__getitem__" of'):
                self.invalid_index_type(arg_type, base, context)
                return 
            
            if name.startswith('"__setitem__" of'):
                if n == 1:
                    self.invalid_index_type(arg_type, base, context)
                else:
                    self.fail(INCOMPATIBLE_TYPES_IN_ASSIGNMENT, context)
                return 
            
            if name.startswith('method "create" of '):
                name = base
            target = 'to {} '.format(name)
        
        msg = None
        if callee.name == '<list>':
            name = callee.name[1:-1]
            msg = '{} item {} has incompatible type {}'.format(
                name[0].upper() + name[1:], n, self.format_simple(arg_type))
        elif callee.name == '<list-comprehension>':
            msg = 'List comprehension has incompatible type {}[]'.format(
                                  strip_quotes(self.format_simple(arg_type)))
        elif callee.name == '<generator>':
            msg = 'Generator has incompatible item type {}'.format(
                                              self.format_simple(arg_type))
        else:
            msg = 'Argument {} {}has incompatible type {}'.format(
                n, target, self.format_simple(arg_type))
        self.fail(msg, context)
    
    def invalid_index_type(self, index_type, base_str, context):
        self.fail('Invalid index type {} for {}'.format(
            self.format(index_type), base_str), context)
    
    def invalid_argument_count(self, callee, num_args, context):
        if num_args < len(callee.arg_types):
            self.too_few_arguments(callee, context)
        else:
            self.too_many_arguments(callee, context)
    
    def too_few_arguments(self, callee, context):
        msg = 'Too few arguments'
        if callee.name:
            msg += ' for {}'.format(callee.name)
        self.fail(msg, context)
    
    def too_many_arguments(self, callee, context):
        msg = 'Too many arguments'
        if callee.name:
            msg += ' for {}'.format(callee.name)
        self.fail(msg, context)
    
    def too_many_positional_arguments(self, callee, context):
        msg = 'Too many positional arguments'
        if callee.name:
            msg += ' for {}'.format(callee.name)
        self.fail(msg, context)

    def unexpected_keyword_argument(self, callee, name, context):
        msg = 'Unexpected keyword argument "{}"'.format(name)
        if callee.name:
            msg += ' for {}'.format(callee.name)
        self.fail(msg, context)            

    def duplicate_argument_value(self, callee, index, context):
        f = 'Function'
        if callee.name:
            f = '{}'.format(callee.name)
        self.fail('{} gets multiple values for keyword argument "{}"'.
                  format(f, callee.arg_names[index]), context)
    
    def does_not_return_value(self, void_type, context):
        """Report an error about a void type in a non-void
        context. The first argument must be a void type. If the void
        type has a source in it, report it in the error message. This
        allows giving messages such as 'Foo does not return a value'.
        """
        if (void_type).source is None:
            self.fail('Function does not return a value', context)
        else:
            self.fail('{} does not return a value'.format(
                capitalize((void_type).source)), context)
    
    def no_variant_matches_arguments(self, overload, context):
        if overload.name():
            self.fail('No overload variant of {} matches argument types'
                      .format(overload.name()), context)
        else:
            self.fail('No overload variant matches argument types', context)
    
    def function_variants_overlap(self, n1, n2, context):
        self.fail('Function signature variants {} and {} overlap'.format(
            n1 + 1, n2 + 1), context)
    
    def invalid_cast(self, target_type, source_type, context):
        if not self.check_void(source_type, context):
            self.fail('Cannot cast from {} to {}'.format(
                self.format(source_type), self.format(target_type)), context)
    
    def incompatible_operator_assignment(self, op, context):
        self.fail('Result type of {} incompatible in assignment'.format(op),
                  context)
    
    def incompatible_value_count_in_assignment(self, lvalue_count, rvalue_count, context):
        if rvalue_count < lvalue_count:
            self.fail('Need {} values to assign'.format(lvalue_count), context)
        elif rvalue_count > lvalue_count:
            self.fail('Too many values to assign', context)
    
    def type_incompatible_with_supertype(self, name, supertype, context):
        self.fail('Type of "{}" incompatible with supertype "{}"'.format(
            name, supertype.name), context)
    
    def signature_incompatible_with_supertype(self, name, supertype, context):
        self.fail('Signature of "{}" incompatible with supertype "{}"'.format(
            name, supertype), context)
    
    def argument_incompatible_with_supertype(self, arg_num, name, supertype, context):
        self.fail('Argument {} of "{}" incompatible with supertype "{}"'
                  .format(arg_num, name, supertype), context)
    
    def return_type_incompatible_with_supertype(self, name, supertype, context):
        self.fail('Return type of "{}" incompatible with supertype "{}"'
                  .format(name, supertype), context)
    
    def method_expected_as_operator_implementation(self, typ, member, context):
        self.fail('Expected operator method "{}" in {}'.format(
            member, self.format(typ)), context)
    
    def boolean_return_value_expected(self, method, context):
        self.fail('Boolean return value expected for method "{}"'.format(
            method), context)
    
    def incompatible_type_application(self, expected_arg_count, actual_arg_count, context):
        if expected_arg_count == 0:
            self.fail('Type application targets a non-generic function',
                      context)
        elif actual_arg_count > expected_arg_count:
            self.fail('Type application has too many types ({} expected)'
                      .format(expected_arg_count), context)
        else:
            self.fail('Type application has too few types ({} expected)'
                      .format(expected_arg_count), context)
    
    def incompatible_array_item_type(self, typ, index, context):
        self.fail('Array item {} has incompatible type {}'.format(
            index, self.format(typ)), context)
    
    def could_not_infer_type_arguments(self, callee_type, n, context):
        if callee_type.name and n > 0:
            self.fail('Cannot infer type argument {} of {}'.format(
                n, callee_type.name), context)
        else:
            self.fail('Cannot infer function type argument', context)
    
    def invalid_var_arg(self, typ, context):
        self.fail('List or tuple expected as variable arguments', context)
    
    def invalid_keyword_var_arg(self, typ, context):
        if isinstance(typ, Instance) and (
                (typ).typ.full_name() == 'builtins.dict'):
            self.fail('Keywords must be strings', context)
        else:
            self.fail('Argument after ** must be a dictionary',
                      context)
    
    def incomplete_type_var_match(self, member, context):
        self.fail('"{}" has incomplete match to supertype type variable'
                  .format(member), context)
    
    def duplicate_interfaces(self, typ, iface):
        self.fail('Class "{}" implements interface "{}" more than once'
                  .format(typ.name(), iface.name()), typ)
    
    def not_implemented(self, msg, context):
        self.fail('Feature not implemented yet ({})'.format(msg), context)
        return Any()
    
    def interface_member_not_implemented(self, typ, iface, member):
        self.fail('"{}" does not implement "{}" defined in "{}"'.format(
            typ.defn.name, member, iface.defn.name), typ)
    
    def undefined_in_superclass(self, member, context):
        self.fail('"{}" undefined in superclass'.format(member), context)
    
    def check_void(self, typ, context):
        """If type is void, report an error such as '.. does not
        return a value' and return True. Otherwise, return False.
        """
        if isinstance(typ, Void):
            self.does_not_return_value(typ, context)
            return True
        else:
            return False


def capitalize(s):
    """Capitalize the first character of a string."""
    if s == '':
        return ''
    else:
        return s[0].upper() + s[1:]


def extract_type( name):
    """If the argument is the name of a method (of form C.m), return
    the type portion in quotes (e.g. "y"). Otherwise, return the string
    unmodified.
    """
    name = re.sub('^"[a-zA-Z0-9_]+" of ', '', name)
    return name


def strip_quotes(s):
    """Strip a double quote at the beginning and end of the string, if any."""
    s = re.sub('^"', '', s)
    s = re.sub('"$', '', s)
    return s
