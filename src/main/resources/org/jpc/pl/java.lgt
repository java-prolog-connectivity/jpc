
:- object(java(_Reference),
	extends(java(_Reference, _))).

	:- info([
		version is 1.0,
		author is 'Sergio Castro and Paulo Moura',
		date is 2014/03/25,
		comment is 'Low level API for calling Java from Logtalk using familiar message sending syntax.',
		parnames is ['Reference']
	]).

:- end_object.



:- object(java).

	:- info([
		version is 1.0,
		author is 'Sergio Castro',
		date is 2014/03/31,
		comment is 'Object encapsulating JPC Java routines that do not require an explicit sender.'
	]).

:- end_object.

