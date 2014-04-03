:- object(atom_term_util).

	:- info([
		version is 1.0,
		author is 'Sergio Castro',
		date is 2014/03/26,
		comment is 'Abstraction of Prolog atom-term non-standard conversion predicates.'
	]).

	:- public(atom_to_term/3).
	:- mode(atom_to_term(+atom, -term, -list), one).
	:- info(atom_to_term/3, [
		comment is 'Converts an atom to the term it represents and also returns a list of bindings.',
		argnames is ['Atom', 'Term', 'Bindings']
	]).

	:- public(term_to_atom/2).
	:- mode(term_to_atom(+atom, -term, -list), one).
	:- info(term_to_atom/2, [
		comment is 'Converts a term to an atom.',
		argnames is ['Term', 'Atom']
	]).

	:- if(current_logtalk_flag(prolog_dialect, xsb)).

		atom_to_term(Atom, Term, Bindings) :-
			{string:read_atom_to_term(Atom, Term, Bindings),
			 listutil:closetail(Bindings)}.

		term_to_atom(Atom, Term, Bindings) :-
			{string:term_to_atom(Atom, Term, Bindings)}.

	:- endif.

:- end_object.
