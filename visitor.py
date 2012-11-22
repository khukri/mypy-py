import nodes


class NodeVisitor:
    """Empty base class for parse tree node visitors.

    The T type argument specifies the return type of the visit
    methods. As all methods defined here return None by default,
    subclasses do not always need to override all the methods.
    """
    
    # Top-level structures
    
    def visit_mypy_file(self, o):
        pass
    
    def visit_import(self, o):
        pass
    def visit_import_from(self, o):
        pass
    def visit_import_all(self, o):
        pass
    
    # Definitions
    
    def visit_func_def(self, o):
        pass
    def visit_overloaded_func_def(self, o):
        pass
    def visit_type_def(self, o):
        pass
    def visit_var_def(self, o):
        pass
    def visit_global_decl(self, o):
        pass
    def visit_decorator(self, o):
        pass
    
    def visit_var(self, o):
        pass
    
    def visit_annotation(self, o):
        pass
    
    # Statements
    
    def visit_block(self, o):
        pass
    
    def visit_expression_stmt(self, o):
        pass
    def visit_assignment_stmt(self, o):
        pass
    def visit_operator_assignment_stmt(self, o):
        pass
    def visit_while_stmt(self, o):
        pass
    def visit_for_stmt(self, o):
        pass
    def visit_return_stmt(self, o):
        pass
    def visit_assert_stmt(self, o):
        pass
    def visit_yield_stmt(self, o):
        pass
    def visit_del_stmt(self, o):
        pass
    def visit_if_stmt(self, o):
        pass
    def visit_break_stmt(self, o):
        pass
    def visit_continue_stmt(self, o):
        pass
    def visit_pass_stmt(self, o):
        pass
    def visit_raise_stmt(self, o):
        pass
    def visit_try_stmt(self, o):
        pass
    def visit_with_stmt(self, o):
        pass
    
    # Expressions
    
    def visit_int_expr(self, o):
        pass
    def visit_str_expr(self, o):
        pass
    def visit_bytes_expr(self, o):
        pass
    def visit_float_expr(self, o):
        pass
    def visit_paren_expr(self, o):
        pass
    def visit_name_expr(self, o):
        pass
    def visit_member_expr(self, o):
        pass
    def visit_call_expr(self, o):
        pass
    def visit_op_expr(self, o):
        pass
    def visit_cast_expr(self, o):
        pass
    def visit_super_expr(self, o):
        pass
    def visit_unary_expr(self, o):
        pass
    def visit_list_expr(self, o):
        pass
    def visit_dict_expr(self, o):
        pass
    def visit_tuple_expr(self, o):
        pass
    def visit_set_expr(self, o):
        pass
    def visit_index_expr(self, o):
        pass
    def visit_type_application(self, o):
        pass
    def visit_func_expr(self, o):
        pass
    def visit_list_comprehension(self, o):
        pass
    def visit_generator_expr(self, o):
        pass
    def visit_slice_expr(self, o):
        pass
    def visit_conditional_expr(self, o):
        pass
    
    def visit_coerce_expr(self, o):
        pass
    def visit_type_expr(self, o):
        pass
    def visit_java_cast(self, o):
        pass
    
    def visit_temp_node(self, o):
        pass
