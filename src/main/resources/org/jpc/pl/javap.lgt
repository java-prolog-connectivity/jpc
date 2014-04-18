:- op(50, xfx, returns).

:- protocol(javap).

	:- info([
		version is 1.0,
		author is 'Sergio Castro',
		date is 2014/03/19,
		comment is 'Protocol for a minimal abstraction of the JPL API for calling Java from Logtalk using familiar message sending syntax.'
	]).
	
	:- public(invoke/1).
	:- mode(invoke(+nonvar), zero_or_one).
	:- info(invoke/1, [
		comment is 'Invokes the passed message in the context of the receiver.',
		argnames is ['Message']
	]).

:- end_protocol.
