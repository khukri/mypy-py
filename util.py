# Utility functions


def short_type( obj):
    """Return the last component of the type name of an object. If obj is None,
    return 'nil'. For example, if obj is 1, return 'int'.
    """
    if obj is None:
        return 'nil'
    t = str(type(obj))
    return t.split('.')[-1].rstrip("'>")


def indent(s, n):
    """Indent all the lines in s (separated by Newlines) by n spaces."""
    s = ' ' * n + s
    s = s.replace('\n', '\n' + ' ' * n)
    return s


def array_repr(a):
    """Return the items of an array converted to strings using Repr."""
    aa = []
    for x in a:
        aa.append(repr(x))
    return aa


def dump_tagged( nodes, tag):
    """Convert an array into a pretty-printed multiline string representation.
    The format is
      tag(
        item1..
        itemN)
    Individual items are formatted like this:
     - arrays are flattened
     - pairs (str : array) are converted recursively, so that str is the tag
     - other items are converted to strings and indented
     """
    a = []
    if tag:
        a.append(tag + '(')
    for n in nodes:
        if isinstance(n, list):
            a.append(dump_tagged(n, None))
        elif isinstance(n, tuple):
            s = dump_tagged(n[1], n[0])
            a.append(indent(s, 2))
        elif n:
            a.append(indent(str(n), 2))
    if tag:
        a[-1] += ')'
    return '\n'.join(a)
