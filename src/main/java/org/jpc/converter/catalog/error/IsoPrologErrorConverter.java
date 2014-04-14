package org.jpc.converter.catalog.error;

import static org.jpc.engine.prolog.PrologConstants.PROLOG_ERROR_FUNCTOR_NAME;

import java.lang.reflect.Type;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.error.IsoPrologError;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class IsoPrologErrorConverter implements FromTermConverter<Compound, IsoPrologError> {

	public static boolean isIsoPrologError(Term errorTerm) {
		return errorTerm.hasFunctor(PROLOG_ERROR_FUNCTOR_NAME, 2);
	}
	
	@Override
	public IsoPrologError fromTerm(Compound term, Type type, Jpc context) {
		if(!isIsoPrologError(term))
			throw new ConversionException();
		return new IsoPrologError(term);
	}
	
}
