:- object(java).

	:- info([
		version is 1.0,
		author is 'Sergio Castro',
		date is 2014/03/31,
		comment is 'Object encapsulating JPC Java routines that do not require an explicit sender.'
	]).

	:- public(forget/1).
	:- mode(forget(-reference), one).
	:- info(forget/1, [
		comment is 'Forgets a reference term.',
		argnames is ['Reference']
	]).	
	forget(Reference) :- jpc(default)::forgetJRefTerm(term(Reference)).

:- end_object.