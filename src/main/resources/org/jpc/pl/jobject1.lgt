:- object(jobject(_Expression),
	extends(jobject(_Expression, _))).

	:- info([
		version is 1.0,
		author is 'Sergio Castro',
		date is 2014/03/25,
		comment is 'Object allowing to interpret a term as a Java expression.',
		parnames is ['Expression']
	]).

	:- public(return/2).
	:- mode(return(?term, -term), zero_or_one).
	:- info(return/2, [
		comment is 'Delegate a message to a Java object and return the Java method return value.',
		argnames is ['Message', 'ReturnValue']]).

	return(Message, ReturnValue) :-
		parameter(1, Expression),
		java::invoke(Expression, Message, ReturnValue).
		
:- end_object.
