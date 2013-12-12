package org.jpc.converter.catalog.error;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.error.PrologError;
import org.jpc.term.Term;

public class PrologErrorConverter implements FromTermConverter<Term, PrologError> {

	@Override
	public PrologError fromTerm(Term term, Type type, Jpc context) {
		return new PrologError(term);
	}
	
}
