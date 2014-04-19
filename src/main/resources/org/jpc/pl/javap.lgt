:- op(50, xfx, returns).



/*
TODO extract here the method protocol from java/0.
*/
:- protocol(javap).

	:- info([
		version is 1.0,
		author is 'Sergio Castro',
		date is 2014/03/19,
		comment is 'Protocol for a minimal abstraction of the API for calling Java from Logtalk.'
	]).
	
	/*
	:- public(invoke/3).
	:- mode(invoke(+nonvar,+nonvar,var), one).
	:- info(invoke/3, [
		comment is 'Invokes the passed message in the context of the reference.',
		argnames is ['Reference','Message','Output']
	]).
*/
:- end_protocol.
