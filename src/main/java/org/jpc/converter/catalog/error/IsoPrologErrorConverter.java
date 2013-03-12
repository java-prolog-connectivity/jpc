package org.jpc.converter.catalog.error;

import static org.jpc.engine.prolog.PrologConstants.PROLOG_ERROR_FUNCTOR;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.JpcConverter;
import org.jpc.error.IsoPrologError;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class IsoPrologErrorConverter extends JpcConverter<IsoPrologError, Compound> {

	public static boolean isIsoPrologError(Term errorTerm) {
		return errorTerm.hasFunctor(PROLOG_ERROR_FUNCTOR, 2);
	}
	
	@Override
	public IsoPrologError fromTerm(Compound term, Type type, Jpc context) {
		if(!isIsoPrologError(term) || !type.equals(IsoPrologError.class))
			throw new JpcConversionException();
		return new IsoPrologError(term);
	}
	
}
