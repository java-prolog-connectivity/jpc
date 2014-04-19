:- object(java,
	implements(javap)).	

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
	forget(Reference) :- eval(jpc(default)::forgetJRefTerm(term(Reference))).

	:- public(eval/1).
	eval(Expression) :- 
		eval(Expression, _).

	:- public(eval/2).
	eval(Expression, Output) :- 
		%jpc_util::check_output_mode(Output),
		jpc_driver::eval(Expression, Output).
		

:- end_object.
