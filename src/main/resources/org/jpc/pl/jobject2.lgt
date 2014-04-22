:- object(jobject(_Expression, _ReturnValue),
	imports(jobject)).

	:- info([
		version is 1.0,
		author is 'Sergio Castro',
		date is 2014/03/25,
		comment is 'Object allowing to interpret a term as a Java expression.',
		parnames is ['Expression', 'ReturnValue']
	]).

	return(Message, ReturnValue) :-
		^^return(Message, ReturnValue),
		parameter(2, ReturnValue).
		
	jself(JObject) :-
		parameter(1, JObject).
	
:- end_object.
