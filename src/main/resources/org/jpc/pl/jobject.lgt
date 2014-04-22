/*load this after the driver logtalk file*/

:- category(jobject,
	implements(forwarding)).

	:- info([
		version is 1.0,
		author is 'Sergio Castro and Paulo Moura',
		date is 2014/03/27,
		comment is 'Forward messages that are not understood by a Logtalk object to Java.']).

	:- public(return/2).
	:- mode(return(?term, -term), zero_or_one).
	:- info(return/2, [
		comment is 'Delegate a message to a Java object and return the Java method return value.',
		argnames is ['Object::Message', 'ReturnValue']]).

	return(Message, ReturnValue) :-
		jself(Object),
		java::invoke(Object, Message, ReturnValue).


	forward(Message) :-
		return(Message, _ReturnValue).


	protected(jself/1).
	:- mode(jself(?term), one).
	:- info(jself/1, [
		comment is 'The term representation of the receiver in the Java side.',
		argnames is ['JObject']]).

	jself(JObject) :-
		self(JObject).
		
:- end_category.
