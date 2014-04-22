:- object(jobject(_Expression, _ReturnValue),
	implements(forwarding)).

	:- info([
		version is 1.0,
		author is 'Sergio Castro',
		date is 2014/03/25,
		comment is 'Object allowing to interpret a term as a Java expression.',
		parnames is ['Expression', 'ReturnValue']
	]).

	forward(Message) :-
		parameter(1, Expression),
		parameter(2, Output),
		java::invoke(Expression, Message, Output).

:- end_object.
