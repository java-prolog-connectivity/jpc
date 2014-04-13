:- protocol(javap).

	:- info([
		version is 1.0,
		author is 'Sergio Castro and Paulo Moura',
		date is 2014/03/19,
		comment is 'Protocol for a minimal abstraction of the JPL API for calling Java from Logtalk using familiar message sending syntax.'
	]).


	:- public(set_field/2).
	:- mode(set_field(+atom, +nonvar), one).
	:- info(set_field/2, [
		comment is 'Sets the value of an object or class field.',
		argnames is ['Field', 'Value']
	]).

	
	:- public(invoke/1).
	:- mode(invoke(+nonvar), zero_or_one).
	:- info(invoke/1, [
		comment is 'Invokes the passed message in the context of the receiver.',
		argnames is ['Message']
	]).

:- end_protocol.
