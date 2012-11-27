import os.path


class ErrorInfo:
    """Representation of a single error message."""
    # Description of a sequence of imports that refer to the source file
    # related to this error. Each item is a (path, line number) tuple.
    import_ctx = None
    # The source file that was the source of this error.
    file = None
    # The name of the type in which this error is located at.
    typ = None     # Unqualified, may be None
    # It the error located within an interface?
    is_interface = None
    # The name of the function or member in which this error is located at.
    function_or_member = None     # Unqualified, may be None
    # The line number related to this error within file.
    line = None     # -1 if unknown
    # The error message.
    message = None
    
    def type_id(self):
        """Return 'interface' or 'class' depending on the context of the error.
        Call this method only if the type member != None.
        """
        if self.is_interface:
            return 'interface'
        else:
            return 'class'
    
    def __init__(self, import_ctx, file, typ, is_interface, function_or_member, line, message):
        self.import_ctx = import_ctx
        self.file = file
        self.typ = typ
        self.is_interface = is_interface
        self.function_or_member = function_or_member
        self.line = line
        self.message = message


class Errors:
    """Container for compile errors.

    This class generates and keeps tracks of compile errors and the
    current error context (nested imports).
    """
    # List of generated error messages.
    error_info = None
    
    # Current error context.
    # Import context, as a list of (path, line) pairs.
    import_ctx = None
    # Path name prefix that is removed from all paths, if set.
    ignore_prefix = None
    file = None     # Path to current file.
    # Short name of current type (or None).
    type_name = None
    # Is the current type an interface?
    is_interface = False
    # Short name of current function or member (or None).
    function_or_member = None
    
    def __init__(self):
        self.error_info = []
        self.import_ctx = []
    
    def set_ignore_prefix(self, prefix):
        """Set path prefix that will be removed from all paths."""
        prefix = os.path.normpath(prefix)
        # Add separator to the end, if not given.
        if os.path.basename(prefix) != '':
            prefix += os.sep
        self.ignore_prefix = prefix
    
    def set_file(self, file):
        """Set the path of the current file."""
        file = os.path.normpath(file)
        self.file = remove_path_prefix(file, self.ignore_prefix)
    
    def set_function(self, name):
        """Set the current function or member short name (it can be None)."""
        self.function_or_member = name
    
    def set_type(self, name, is_interface):
        """Set the short name of the current type (it can be None)."""
        self.type_name = name
        self.is_interface = is_interface
    
    def push_import_context(self, path, line):
        """Add a (file, line) tuple to the import context."""
        self.import_ctx.append((os.path.normpath(path), line))
    
    def pop_import_context(self):
        """Remove the topmost item from the import context."""
        self.import_ctx.pop()
    
    def import_context(self):
        """Return a copy of the import context."""
        return self.import_ctx[:]
    
    def set_import_context(self, ctx):
        """Replace the entire import context with a new value."""
        self.import_ctx = ctx[:]
    
    def report(self, line, message):
        """Report a message at the given line using the current error
        context."""
        info = ErrorInfo(self.import_context(), self.file, self.type_name,
                         self.is_interface, self.function_or_member, line,
                         message)
        self.error_info.append(info)
    
    def num_messages(self):
        """Return the number of generated messages."""
        return len(self.error_info)
    
    def is_errors(self):
        """Are there any generated errors?"""
        return len(self.error_info) > 0
    
    def raise_error(self):
        """Raise a CompileError with the generated messages. Render
        the messages suitable for displaying.
        """
        raise CompileError(self.messages())
    
    def messages(self):
        """Return a string array that represents the error messages in a form
        suitable for displaying to the user.
        """
        a = []
        errors = self.render_messages(self.sort_messages(self.error_info))
        errors = self.remove_duplicates(errors)
        for file, line, message in errors:
            s = None
            if file is not None:
                if line is not None and line >= 0:
                    s = '{}, line {}: {}'.format(file, line, message)
                else:
                    s = '{}: {}'.format(file, message)
            else:
                s = message
            a.append(s)
        return a
    
    def render_messages(self, errors):
        """Translate the messages into a sequence of (path, line,
        message) tuples.  The rendered sequence includes information
        about error contexts. The path item may be None. If the line
        item is negative, the line number is not defined for the
        tuple.
        """
        result = [] # (path, line, message)
        
        prev_import_context = []
        prev_function_or_member = None
        prev_type = None
        
        for e in errors:
            # Report module import context, if different from previous message.
            if e.import_ctx != prev_import_context:
                last = len(e.import_ctx) - 1
                i = last
                while i >= 0:
                    path, line = e.import_ctx[i]
                    fmt = 'In module imported in {}, line {}'
                    if i < last:
                        fmt = '                   in {}, line {}'
                    if i > 0:
                        fmt += ','
                    else:
                        fmt += ':'
                    # Remove prefix to ignore from path (if present) to
                    # simplify path.
                    path = remove_path_prefix(path, self.ignore_prefix)
                    result.append((None, -1, fmt.format(path, line)))
                    i -= 1
            
            # Report context within a source file.
            if (e.function_or_member != prev_function_or_member or
                    e.typ != prev_type):
                if e.function_or_member is None:
                    if e.typ is None:
                        result.append((e.file, -1, 'At top level:'))
                    else:
                        result.append((e.file, -1, 'In {} "{}":'.format(
                            e.type_id(), e.typ)))
                else:
                    if e.typ is None:
                        result.append((e.file, -1,
                                       'In function "{}":'.format(
                                           e.function_or_member)))
                    else:
                        result.append((e.file, -1,
                                       'In member "{}" of {} "{}":'.format(
                                           e.function_or_member, e.type_id(),
                                           e.typ)))
            elif e.typ != prev_type:
                if e.typ is None:
                    result.append((e.file, -1, 'At top level:'))
                else:
                    result.append((e.file, -1,
                                   'In {} "{}":'.format(e.type_id(), e.typ)))
            
            result.append((e.file, e.line, e.message))
            
            prev_import_context = e.import_ctx
            prev_function_or_member = e.function_or_member
            prev_type = e.typ
        
        return result
    
    def sort_messages(self, errors):
        """Sort an array of error messages locally by line number, i.e. sort a
        run of consecutive messages with the same file context by line number,
        but otherwise retain the general ordering of the messages.
        """
        result = []
        i = 0
        while i < len(errors):
            i0 = i
            # Find neighbouring errors with the same context and file.
            while (i + 1 < len(errors) and
                       errors[i + 1].import_ctx == errors[i].import_ctx and
                       errors[i + 1].file == errors[i].file):
                i += 1
            i += 1
            
            # Sort the errors specific to a file according to line number.
            a = stable_sort(errors[i0:i], lambda x: x.line)
            result.extend(a)
        return result
    
    def remove_duplicates(
                                  self, errors):
        """Remove duplicates from a sorted error list."""
        res = []
        i = 0
        while i < len(errors):
            dup = False
            j = i - 1
            while (j >= 0 and errors[j][0] == errors[i][0] and
                       errors[j][1] == errors[i][1]):
                if errors[j] == errors[i]:
                    dup = True
                    break
                j -= 1
            if not dup:
                res.append(errors[i])
            i += 1
        return res


class CompileError(Exception):
    """Exception raised when there is a parse, semantic analysis, type check or
    other compilation-related error.
    """
    messages = None
    
    def __init__(self, messages):
        super().__init__()
        self.messages = messages


def stable_sort(a, key):
    """Perform a stable sort of a sequence, i.e. if the original sequence has
    a[n] == a[n+m] (when comparing using the comparison function f), in the
    sorted sequence item a[n] will be at an earlier index than a[n + m].
    """
    # TODO use sorted with key (need support for keyword arguments)
    l = []
    for i, x in enumerate(a):
        l.append((key(x), i, x))
    result = []
    for k, j, y in sorted(l):
        result.append(y)        
    return result


def remove_path_prefix( path, prefix):
    """If path starts with prefix, return copy of path with the prefix removed.
    Otherwise, return path. If path is None, return None.
    """
    if prefix is not None and path.startswith(prefix):
        return path[len(prefix):]
    else:
        return path
