:- object(jref_term(_Id),
	imports(java_bridge)).

:- end_object.


:- object(jserialized(_Serialization),
	imports(java_bridge)).

:- end_object.


:- object(jreflective(_Object),
	imports(java_bridge)).

:- end_object.


:- object(jtyped(_Object, _Type),
	imports(java_bridge)).

:- end_object.


:- object(jconvertable(_Object, _Converter),
	imports(java_bridge)).

:- end_object.


:- object(term(_Term),
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
