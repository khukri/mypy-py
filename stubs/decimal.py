# bug: sometimes __mycache__ is not updated automatically
#import _thread
import builtins
import collections

Context BasicContext
class Clamped(DecimalException): pass
class ConversionSyntax(InvalidOperation): pass
class Context(builtins.object):
	int prec
	str rounding # TODO enum?
	list<DecimalException> traps
	list<DecimalException> flags
	int Emin
	int Emax
	int capitals # wtf, should be bool
	int clamp # wtf, should be bool
	any Etiny(Context self): pass
	any Etop(Context self): pass
	Context __copy__(Context self): pass
	void __init__(Context self): pass
	void __init__(Context self, int prec, str rounding): pass
	void __init__(Context self, int prec, str rounding, list<DecimalException> traps): pass
	void __init__(Context self, int prec, str rounding, list<DecimalException> traps, list<DecimalException> flags): pass
	void __init__(Context self, int prec, str rounding, list<DecimalException> traps, list<DecimalException> flags, int Emin): pass
	void __init__(Context self, int prec, str rounding, list<DecimalException> traps, list<DecimalException> flags, int Emin, int Emax): pass
	void __init__(Context self, int prec, str rounding, list<DecimalException> traps, list<DecimalException> flags, int Emin, int Emax, int capitals): pass
	void __init__(Context self, int prec, str rounding, list<DecimalException> traps, list<DecimalException> flags, int Emin, int Emax, int capitals, int clamp): pass
	void __init__(Context self, int prec, str rounding, list<DecimalException> traps, list<DecimalException> flags, int Emin, int Emax, int capitals, int clamp, any _ignored_flags): pass
	str __repr__(Context self): pass
	Decimal abs(Context self, Decimal a): pass
	Decimal add(Context self, Decimal a, Decimal b): pass
	Decimal canonical(Context self, Decimal a): pass
	void clear_flags(Context self): pass
	Decimal compare(Context self, Decimal a, Decimal b): pass
	Decimal compare(Context self, int a, int b): pass
	Decimal compare(Context self, Decimal a, int b): pass
	Decimal compare(Context self, int a, Decimal b): pass
	Decimal compare_signal(Context self, Decimal a, Decimal b): pass
	Decimal compare_signal(Context self, int a, int b): pass
	Decimal compare_signal(Context self, Decimal a, int b): pass
	Decimal compare_signal(Context self, int a, Decimal b): pass
	Decimal compare_total(Context self, Decimal a, Decimal b): pass
	Decimal compare_total(Context self, int a, int b): pass
	Decimal compare_total(Context self, Decimal a, int b): pass
	Decimal compare_total(Context self, int a, Decimal b): pass
	Decimal compare_total_mag(Context self, Decimal a, Decimal b): pass
	Decimal compare_total_mag(Context self, int a, int b): pass
	Decimal compare_total_mag(Context self, Decimal a, int b): pass
	Decimal compare_total_mag(Context self, int a, Decimal b): pass
	Context copy(Context self): pass
	Decimal copy_abs(Context self, Decimal a): pass
	Decimal copy_abs(Context self, int a): pass
	Decimal copy_decimal(Context self, int a): pass
	Decimal copy_decimal(Context self, Decimal a): pass
	Decimal copy_negate(Context self, Decimal a): pass
	Decimal copy_negate(Context self, int a): pass
	Decimal copy_sign(Context self, Decimal a): pass
	Decimal copy_sign(Context self, int a): pass
	Decimal create_decimal(Context self, str num): pass
	Decimal create_decimal(Context self, float num): pass
	Decimal create_decimal(Context self, int num): pass
	Decimal create_decimal(Context self, Decimal num): pass	
	Decimal create_decimal_from_float(float f): pass
	Decimal create_decimal_from_float(int f): pass
	Decimal divide(Context self, Decimal a, Decimal b): pass
	Decimal divide(Context self, int a, int b): pass
	Decimal divide(Context self, Decimal a, int b): pass
	Decimal divide(Context self, int a, Decimal b): pass
	Decimal divide_int(Context self, Decimal a, Decimal b): pass
	Decimal divide_int(Context self, int a, int b): pass
	Decimal divide_int(Context self, Decimal a, int b): pass
	Decimal divide_int(Context self, int a, Decimal b): pass
	Decimal divmod(Context self, Decimal a, Decimal b): pass
	Decimal divmod(Context self, int a, int b): pass
	Decimal divmod(Context self, Decimal a, int b): pass
	Decimal divmod(Context self, int a, Decimal b): pass
class DecimalException(builtins.ArithmeticError): # TODO test
	void handle(object self, Context context): pass
	void handle(object self, Context context, int sign): pass
class DecimalTuple(builtins.tuple):
	tuple __getnewargs__(DecimalTuple self): pass
	str __repr__(DecimalTuple self): pass
	collections.OrderedDict _asdict(DecimalTuple self): pass
	DecimalTuple _replace(DecimalTuple _self, any **kwds): pass
	#DecimalTuple _make(DecimalTuple cls, Iterator iterable, func<type> new, func<int> len)
Context DefaultContext
class DivisionByZero: pass
class DivisionImpossible: pass
class DivisionUndefined: pass
Context ExtendedContext
class Inexact: pass
class InvalidContext: pass
class InvalidOperation(DecimalException): pass
class Overflow: pass
str ROUND_05UP
str ROUND_CEILING
str ROUND_DOWN
str ROUND_FLOOR
str ROUND_HALF_DOWN
str ROUND_HALF_EVEN
str OUND_HALF_UP
str ROUND_UP
class Rounded: pass
class Subnormal: pass
class Underflow: pass
class _ContextManager: pass
Decimal _Infinity
class _Log10Memoize: pass
Decimal _NaN
Decimal _NegativeInfinity
Decimal _NegativeOne
Decimal _One
int _PyHASH_10INV
int _PyHASH_INF
int _PyHASH_MODULUS
int _PyHASH_NAN
tuple<Decimal, Decimal> _SignedInfinity
class _WorkRep: pass
Decimal _Zero
str __cached__
str __doc__
str __file__
void __package__
str __version__
#_sre.SRE_Pattern.match _all_zeros
#dict _condition_map
def _convert_for_comparison(): pass
def _convert_other(): pass
#any _copy
def _dec_from_triple(): pass
def _dexp(): pass
def _div_nearest(): pass
def _dlog(): pass
def _dlog10(): pass
def _dpower(): pass
#_sre.SRE_Pattern.match _exact_half
def _format_align(): pass
def _format_number(): pass
def _format_sign(): pass
def _group_lengths(): pass
def _iexp(): pass
def _ilog(): pass
def _insert_thousands_sep(): pass
#any _locale
int _log10_digits(_Log10Memoize self, int p): pass
def _log10_lb(): pass
#any _math
def _namedtuple(): pass
int _nbits(int self): pass
def _normalize(): pass
#any _numbers
def _parse_format_specifier(): pass
#_sre.SRE_Pattern _parse_format_specifier_regex
#_sre.SRE_Pattern.match _parser
def _rshift_nearest(): pass
any _signals
def _sqrt_nearest(): pass
Context localcontext(): pass
#Context localcontext(threading.Thread thread): pass
#def setcontext(Context context, _thread._local _local): pass


class Decimal:
	Decimal add(Decimal self): pass
	int adjusted(Decimal self): pass
	DecimalTuple as_tuple(Decimal self): pass
	Decimal canonical(Decimal self): pass
	Decimal compare(Decimal other): pass
	Decimal compare(Decimal other, Context context): pass
	Decimal compare_signal(Decimal other): pass
	Decimal compare_signal(Decimal other, Context context): pass
	Decimal compare_total_mag(Decimal other): pass
	Decimal conjugate(Decimal self): pass
	Decimal copy_abs(Decimal self): pass
	Decimal copy_negate(Decimal self): pass
	Decimal exp(Decimal self): pass
	Decimal from_float(Decimal self, float f): pass
	Decimal fma(Decimal self, Decimal other, Decimal third): pass
	Decimal fma(Decimal self, Decimal other, Decimal third, Context context): pass
	bool is_canonical(Decimal self): pass
	bool is_infinite(Decimal self): pass
	bool is_nan(Decimal self): pass
	bool is_normal(Decimal self): pass
	bool is_qnan(Decimal self): pass
	bool is_signed(Decimal self): pass
	bool is_snan(Decimal self): pass
	bool is_subnormal(Decimal self): pass
	bool is_zero(Decimal self): pass
	Decimal logb(Decimal self): pass
	Decimal truediv(Decimal a, Decimal b): pass # todo int and float too
	Decimal floordiv(Decimal a, Decimal b): pass
	bool is_(Decimal a, Decimal b): pass
	Decimal exp(Decimal self): pass
	Decimal exp(Decimal self, Context context): pass

Decimal remove_exponent(str d): pass

Context getcontext(): pass


