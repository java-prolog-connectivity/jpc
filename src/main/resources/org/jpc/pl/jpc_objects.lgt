:- object(jpc(_Id),
	imports(java_bridge)).

:- end_object.



:- object([_| _],
	imports(java_bridge)).

/*
	:- public(my_get/2).

	my_get(X,Y) :- ::get(X) returns Y.
*/

:- end_object.


:- object('[]',
	imports(java_bridge)).

:- end_object.
