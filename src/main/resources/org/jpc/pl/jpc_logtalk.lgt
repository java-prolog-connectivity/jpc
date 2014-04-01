:- protocol(javap).

	:- info([
		version is 1.0,
		author is 'Sergio Castro and Paulo Moura',
		date is 2014/03/19,
		comment is 'Protocol for a minimal abstraction of the JPL API for calling Java from Logtalk using familiar message sending syntax.'
	]).

	:- public(get_field/2).
	:- mode(get_field(+atom, ?nonvar), zero_or_one).
	:- info(get_field/2, [
		comment is 'Gets the value of a class field.',
		argnames is ['Field', 'Value']
	]).

	:- public(set_field/2).
	:- mode(set_field(+atom, +nonvar), one).
	:- info(set_field/2, [
		comment is 'Sets the value of a class field.',
		argnames is ['Field', 'Value']
	]).

	
	:- public(invoke/1).
	:- mode(invoke(+nonvar), zero_or_one).
	:- info(invoke/1, [
		comment is 'Invokes the passed message in the context of the receiver.',
		argnames is ['Message']
	]).

/*
	:- public(forget/1).
	:- mode(forget(-reference), one).
	:- info(forget/1, [
		comment is 'Forgets a reference.',
		argnames is ['Reference']
	]).	
*/

:- end_protocol.
