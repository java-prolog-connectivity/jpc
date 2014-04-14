package org.jpc.converter.catalog.error;

import static org.jpc.converter.catalog.error.IsoPrologErrorConverter.isIsoPrologError;

import java.lang.reflect.Type;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.error.InstantiationError;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class InstantiationErrorConverter implements FromTermConverter<Compound, InstantiationError> {

	public static final String INSTANTIATION_ERROR_FUNCTOR_NAME = "instantiation_error";
	
	public static boolean isInstantiationError(Term term) {
		return isIsoPrologError(term) && term.arg(1).hasFunctor(INSTANTIATION_ERROR_FUNCTOR_NAME, 0);
	}
	
	@Override
	public InstantiationError fromTerm(Compound term, Type type, Jpc context) {
		if(!isInstantiationError(term))
			throw new ConversionException();
		return new InstantiationError(term);
	}
	
}
