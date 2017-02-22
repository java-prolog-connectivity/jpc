package org.jpc.converter.catalog.error;

import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.error.PrologError;
import org.jpc.term.Term;

public class PrologErrorConverter implements FromTermConverter<Term, PrologError> {

	@Override
	public PrologError fromTerm(Term term, TypeDomain target, Jpc context) {
		return new PrologError(term);
	}
	
}
