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
	:- mode(set_field(+atom, +nonvar), zero_or_one).
	:- info(set_field/2, [
		comment is 'Sets the value of a class field.',
		argnames is ['Field', 'Value']
	]).

	:- public(new/2).
	:- mode(new(+list(nonvar), -reference), one).
	:- info(new/2, [
		comment is 'Creates a new instance using the specified parameter values.',
		argnames is ['Parameters', 'Instance']
	]).

	:- public(new/1).
	:- mode(new(-reference), one).
	:- info(new/1, [
		comment is 'Creates a new instance using default parameter values.',
		argnames is ['Instance']
	]).

	:- public(forget/1).
	:- mode(forget(-reference), one).
	:- info(forget/1, [
		comment is 'Forgets a reference.',
		argnames is ['Reference']
	]).
	
	:- public(invoke/2).
	:- mode(invoke(+atom, +list(nonvar)), one).
	:- info(invoke/2, [
		comment is 'Creates a new instance using the specified parameter values.',
		argnames is ['Reference', 'Parameters']
	]).

:- end_protocol.