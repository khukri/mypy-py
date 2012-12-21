import builtins

class Error(builtins.Exception):
	list __weakref__

# TODO: strictly speaking we should only allow objects with __copy__ and __deepcopy__ methods
# (not sure how to express this in code yet)

object copy(object x): pass

object deepcopy(object x): pass
object deepcopy(object x, dict memo): pass
object deepcopy(object x, dict memo, list _nil): pass
