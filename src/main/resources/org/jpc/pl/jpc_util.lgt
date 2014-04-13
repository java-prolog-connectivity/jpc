:- object(jpc_util).

	:-public(check_output_mode/1).
	check_output_mode(Output) :- var(Output), Output = term(_).
	check_output_mode(Output) :- \+ var(Output).
	
:- end_object.
