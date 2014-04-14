:- object(jref_term(_Id),
	imports(java_bridge)).

:- end_object.


:- object(jserialized(_Id),
	imports(java_bridge)).

:- end_object.


:- object(class(_PackageNames,_ClassNameComponents),
	imports(java_bridge)).

:- end_object.


:- object(reflective(_Reference),
	imports(java_bridge)).

:- end_object.


:- object([_| _],
	imports(java_bridge)).

:- end_object.


:- object('[]',
	imports(java_bridge)).

:- end_object.
