:- object(robject(_Object, _ReturnValue),
	extends(jobject(_Object, _ReturnValue))).

	return(invoke(Message), ReturnValue) :-
		^^return(Message, ReturnValue).

	return(set(FieldName, Value), ReturnValue) :-
		^^return([FieldName, Value], ReturnValue).

	return(get(FieldName), ReturnValue) :-
		^^return([FieldName], ReturnValue).

:- end_object.


:- object(robject(_Object),
	extends(robject(_Object,_ReturnValue))).

:- end_object.



:- object(jref_term(_Id),
	imports(jobject)).

:- end_object.


:- object(jserialized(_Serialization),
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


:- object(class(_FullyQualifiedName),
	imports(jobject)).

:- end_object.


:- object(class(_PackageNames,_ClassNameComponents),
	imports(jobject)).

:- end_object.


:- object(type(_FullyQualifiedName),
	imports(jobject)).

:- end_object.


:- object(type(_PackageNames,_ClassNameComponents),
	imports(jobject)).

:- end_object.


:- object(type(_PackageNames,_ClassNameComponents, _ActualTypeArguments, _OwnerType),
	imports(jobject)).

:- end_object.


:- object(array(_ComponentType),
	imports(jobject)).

:- end_object.


:- object(variable_type(_Name, _GenericDeclaration, _UpperBounds),
	imports(jobject)).

:- end_object.


:- object(variable_type(_UpperBounds, _LowerBounds),
	imports(jobject)).

:- end_object.


:- object([_| _],
	imports(jobject)).

:- end_object.


:- object('[]',
	imports(jobject)).

:- end_object.
