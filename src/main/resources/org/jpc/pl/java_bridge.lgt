:- op(50, xfx, returns).


/*load this after the driver logtalk file*/

:- category(java_bridge,
	implements(forwarding)).

	:- info([
		version is 1.0,
		author is 'Sergio Castro and Paulo Moura',
		date is 2014/03/27,
		comment is 'Forward messages that are not understood by a Logtalk object to Java.']).

	:- public(returns/2).
	:- mode(returns(?term, -term), zero_or_one).
	:- info(returns/2, [
		comment is 'Delegate a message to a Java object and return the Java method return value.',
		argnames is ['Object::Message', 'ReturnValue']]).

	returns(Message, ReturnValue) :-
		self(Object),
		java(Object, ReturnValue)::invoke(Message).

	forward(Message) :-
		returns(Message, _ReturnValue).

:- end_category.
