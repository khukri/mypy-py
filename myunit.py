import sys
import re
import time
import traceback


# TODO remove global state
is_verbose = False
is_quiet = False
patterns = []
times = []


class AssertionFailure(Exception):
    def __init__(self, s=None):
        if s:
            super().__init__(s)
        else:
            super().__init__()


class SkipTestCaseException(Exception): pass


def assert_true(b, msg=None):
    """Exception used to signal skipped test cases."""
    if not b:
        raise AssertionFailure(msg)


def assert_equal(a, b, fmt='{} != {}'):
    if a != b:
        raise AssertionFailure(fmt.format(repr(a), repr(b)))


def assert_not_equal(a, b, fmt='{} == {}'):
    if a == b:
        raise AssertionFailure(fmt.format(repr(a), repr(b)))


def assert_raises( typ, *rest):
    """Usage: assert_raises(exception class[, message], function[, args])
    
    Call function with the given arguments and expect an exception of the given
    type.
    
    TODO use overloads for better type checking
    """
    # Parse arguments.
    msg = None
    if isinstance(rest[0], str) or rest[0] is None:
        msg = rest[0]
        rest = rest[1:]
    f = rest[0]
    args = []
    if len(rest) > 1:
        args = rest[1]
        assert len(rest) <= 2
    
    # Perform call and verify the exception.
    try:
        f(*args)
    except Exception as e:
        assert_type(typ, e)
        if msg:
            assert_equal(e.args[0], msg, 'Invalid message {}, expected {}')
    else:
        raise AssertionFailure('No exception raised')


def assert_type( typ, value):
    if type(value) != typ:
        raise AssertionFailure('Invalid type {}, expected {}'.format(
            typename(type(value)), typename(typ)))


def fail():
    raise AssertionFailure()


class TestCase:
    def __init__(self, name, suite=None, func=None):
        self.func = func
        self.name = name
        self.suite = suite
    
    def run(self):
        if self.func:
            self.func()
    
    def set_up(self):
        if self.suite:
            self.suite.set_up()
    
    def tear_down(self):
        if self.suite:
            self.suite.tear_down()


class Suite:
    def __init__(self):
        self.prefix = typename(type(self)) + '.'
        # Each test case is either a TestCase object or (str, function).
        self._test_cases = []
        self.init()
    
    def set_up(self):
        pass
    
    def tear_down(self):
        pass
    
    def init(self):
        for m in dir(self):
            if m.startswith('test'):
                t = getattr(self, m)
                if isinstance(t, Suite):
                    self.add_test((m + '.', t))
                else:
                    self.add_test(TestCase(m, self, getattr(self, m)))
    
    def add_test(self, test):
            
        def add_test1(self, test):
            self._test_cases.append(test)
        
        def add_test2(self, test):
            self._test_cases.append(test)
    
        if isinstance(test, TestCase):
            return add_test1(self, test)
        elif isinstance(test, tuple):
            return add_test2(self, test)
        else:
            raise TypeError("Invalid argument types")
    
    def cases(self):
        return self._test_cases[:]
    
    def skip(self):
        raise SkipTestCaseException()


def run_test(t, args=None):
    global patterns, is_verbose, is_quiet
    if not args:
        args = []
    is_verbose = False
    is_quiet = False
    patterns = []
    i = 0
    while i < len(args):
        a = args[i]
        if a == '-v':
            is_verbose = True
        elif a == '-q':
            is_quiet = True
        elif len(a) > 0 and a[0] != '-':
            patterns.append(a)
        else:
            raise ValueError('Invalid arguments')
        i += 1
    if len(patterns) == 0:
        patterns.append('*')
    
    num_total, num_fail, num_skip = run_test_recursive(t, 0, 0, 0, '', 0)
    
    skip_msg = ''
    if num_skip > 0:
        skip_msg = ', {} skipped'.format(num_skip)
    
    if num_fail == 0:
        if not is_quiet:
            print('%d test cases run%s, all passed.' % (num_total, skip_msg))
            print('*** OK ***')
    else:
        sys.stderr.write('%d/%d test cases failed%s.\n' % (num_fail,
                                                           num_total,
                                                           skip_msg))
        sys.stderr.write('*** FAILURE ***\n')


def run_test_recursive( test, num_total, num_fail, num_skip, prefix, depth):
    """The first argument may be TestCase, Suite or (Str, Suite)."""
    if isinstance(test, TestCase):
        name = prefix + test.name
        for pattern in patterns:
            if match_pattern(name, pattern):
                match = True
                break
        else:
            match = False
        if match:
            is_fail, is_skip = run_single_test(name, test)
            if is_fail: num_fail += 1
            if is_skip: num_skip += 1
            num_total += 1
    else:
        suite = None
        suite_prefix = None
        if isinstance(test, list) or isinstance(test, tuple):
            suite = test[1]
            suite_prefix = test[0]
        else:
            suite = test
            suite_prefix = test.prefix
        
        for stest in suite.cases():
            new_prefix = prefix
            if depth > 0:
                new_prefix = prefix + suite_prefix
            num_total, num_fail, num_skip = run_test_recursive(
                stest, num_total, num_fail, num_skip, new_prefix, depth + 1)
    return num_total, num_fail, num_skip


def run_single_test( name, test):
    if is_verbose:
        sys.stderr.write(name)

    time0 = time.time()
    test.set_up() # FIX: check exceptions
    try:
        test.run()
    except Exception:
        exc_type, exc_value, exc_traceback = sys.exc_info()
    else:
        exc_traceback = None
    test.tear_down() # FIX: check exceptions
    times.append((time.time() - time0, name))

    if exc_traceback:
        if isinstance(exc_value, SkipTestCaseException):
            if is_verbose:
                sys.stderr.write(' (skipped)\n')
            return False, True
        else:
            handle_failure(name, exc_type, exc_value, exc_traceback)
            return True, False
    elif is_verbose:
        sys.stderr.write('\n')
        
    return False, False


def handle_failure(name, exc_type, exc_value, exc_traceback):
    # Report failed test case.
    if is_verbose:
        sys.stderr.write('\n\n')
    msg = None
    if exc_value.args and exc_value.args[0]:
        msg = ': ' + exc_value.args[0]
    else:
        msg = ''
    sys.stderr.write('Traceback (most recent call last):\n')
    tb = traceback.format_tb(exc_traceback)
    tb = clean_traceback(tb)
    for s in tb:
        sys.stderr.write(s)
    exception = typename(exc_type)
    sys.stderr.write('{}{}\n\n'.format(exception, msg))
    sys.stderr.write('{} failed\n\n'.format(name))


def typename(t):
    if '.' in str(t):
        return str(t).split('.')[-1].rstrip("'>")
    else:
        return str(t)[8:-2]


def match_pattern(s, p):
    if len(p) == 0:
        return len(s) == 0
    elif p[0] == '*':
        if len(p) == 1:
            return True
        else:
            for i in range(len(s) + 1):
                if match_pattern(s[i:], p[1:]):
                    return True
            return False
    elif len(s) == 0:
        return False
    else:
        return s[0] == p[0] and match_pattern(s[1:], p[1:])


def clean_traceback( tb):
    # Remove clutter from the traceback.
    start = 0
    for i, s in enumerate(tb):
        if '\n    test.run()\n' in s or '\n    self.func()\n' in s:
            start = i + 1
    tb = tb[start:]
    for f in ['assert_equal', 'assert_not_equal', 'assert_type',
              'assert_raises', 'assert_true']:
        if tb != [] and ', in {}\n'.format(f) in tb[-1]:
            tb = tb[:-1]
    return tb
