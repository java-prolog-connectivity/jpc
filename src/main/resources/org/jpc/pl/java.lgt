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
		


	preprocess_message(Message1, Message1, Output) :-
		\+ var(Output).
		
	preprocess_message(Message1, Message2, Output) :-
		var(Output), 
		decompose_message(Message1, Name, Args1),
		return_specifier(Args1, Args2, Output),
		Message2 =..[Name|Args2].
		
	decompose_message(Message, Message, []) :-
		atom(Message).
		
	decompose_message(Message, Name, Args) :-
		compound(Message),
		Message =.. [Name|Args].
	
	return_specifier([],[],_).
	
	return_specifier([H|Rest1],[H|Rest2],Output) :- 
		\+ H=return(_), 
		return_specifier(Rest1,Rest2,Output).
	
	return_specifier([return(Ret)|Rest],Rest,Ret).
		
	:- public(invoke/2).
	invoke(Receiver, Message) :-
		invoke(Receiver, Message, _).
		
	:- public(invoke/3).
	invoke(Receiver, Message, Output) :-
		preprocess_message(Message, MessageProcessed, Output),
		LogtalkCall = Receiver::MessageProcessed,
		eval(LogtalkCall, Output).

	:- public(get_field/3).
	get_field(Receiver, FieldName, FieldValue) :-
		eval(Receiver::[FieldName], FieldValue).
		

	:- public(set_field/3).
	set_field(Receiver, FieldName, FieldValue) :-
		eval(Receiver::[FieldName, FieldValue]).

:- end_object.
