:- object(java(_Reference, _ReturnValue),
	implements(forwarding, javap)).

	:- info([
		version is 1.0,
		author is 'Sergio Castro',
		date is 2014/03/25,
		comment is 'Low level API for calling Java from Logtalk using familiar message sending syntax.',
		parnames is ['Reference', 'ReturnValue']
	]).

	check_output_mode(Output) :- var(Output), Output = term(_).
	check_output_mode(Output) :- \+ var(Output).
		
	invoke(Message) :-
		parameter(1, Reference),
		parameter(2, Output),
		check_output_mode(Output),
		LogtalkCall = Reference::Message,
		jpc_driver::eval(LogtalkCall, Output).


	forward(Message) :-
		invoke(Message).

:- end_object.
