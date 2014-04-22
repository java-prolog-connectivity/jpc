:- object(jref_term(_Id),
	imports(jobject)).

:- end_object.


:- object(jserialized(_Serialization),
	imports(jobject)).

:- end_object.


:- object(jreflective(_Object),
	imports(jobject)).

:- end_object.


:- object(jtyped(_Object, _Type),
	imports(jobject)).

:- end_object.


:- object(jconvertable(_Object, _Converter),
	imports(jobject)).

:- end_object.


:- object(term(_Term),
	imports(jobject)).

:- end_object.


:- object(class(_PackageNames,_ClassNameComponents),
	imports(jobject)).

:- end_object.


:- object(reflective(_Reference),
	imports(jobject)).

:- end_object.


:- object([_| _],
	imports(jobject)).

:- end_object.


:- object('[]',
	imports(jobject)).

:- end_object.
