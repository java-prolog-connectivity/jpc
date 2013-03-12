package org.jpc.converter.catalog.error;

import static org.jpc.converter.catalog.error.IsoPrologErrorConverter.isIsoPrologError;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.JpcConverter;
import org.jpc.error.InstantiationError;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class InstantiationErrorConverter extends JpcConverter<InstantiationError, Compound> {

	public static final String INSTANTIATION_ERROR_FUNCTOR = "instantiation_error";
	
	public static boolean isInstantiationError(Term term) {
		return isIsoPrologError(term) && term.arg(1).hasFunctor(INSTANTIATION_ERROR_FUNCTOR, 0);
	}
	
	@Override
	public InstantiationError fromTerm(Compound term, Type type, Jpc context) {
		if(!isInstantiationError(term) || !type.equals(InstantiationError.class))
			throw new JpcConversionException();
		return new InstantiationError(term);
	}
	
}
