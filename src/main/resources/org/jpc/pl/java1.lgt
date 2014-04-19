:- object(java(_Reference),
	extends(java(_Reference, _))).

	:- info([
		version is 1.0,
		author is 'Sergio Castro',
		date is 2014/03/25,
		comment is 'Low level API for calling Java from Logtalk using familiar message sending syntax.',
		parnames is ['Reference']
	]).

	:- public(returns/2).
	:- mode(returns(?term, -term), zero_or_one).
	:- info(returns/2, [
		comment is 'Delegate a message to a Java object and return the Java method return value.',
		argnames is ['Object::Message', 'ReturnValue']]).

	returns(Message, ReturnValue) :-
		parameter(1, Object),
		java::invoke(Object, Message, ReturnValue).
		
:- end_object.

