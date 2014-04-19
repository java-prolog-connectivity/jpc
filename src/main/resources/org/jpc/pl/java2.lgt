:- object(java(_Reference, _ReturnValue),
	implements(forwarding)).

	:- info([
		version is 1.0,
		author is 'Sergio Castro',
		date is 2014/03/25,
		comment is 'Low level API for calling Java from Logtalk using familiar message sending syntax.',
		parnames is ['Reference', 'ReturnValue']
	]).

	forward(Message) :-
		parameter(1, Reference),
		parameter(2, Output),
		java::invoke(Reference, Message, Output).

:- end_object.
