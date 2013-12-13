package org.jpc.converter.catalog.error;

import static org.jpc.converter.catalog.error.IsoPrologErrorConverter.isIsoPrologError;

import java.lang.reflect.Type;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.error.ExistenceError;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class ExistenceErrorConverter implements FromTermConverter<Compound, ExistenceError> {

	public static final String EXISTENCE_ERROR_FUNCTOR = "existence_error";
	
	public static boolean isExistenceError(Term term) {
		return isIsoPrologError(term) && term.arg(1).hasFunctor(EXISTENCE_ERROR_FUNCTOR, 2);
	}
	
	@Override
	public ExistenceError fromTerm(Compound term, Type type, Jpc context) {
		if(!isExistenceError(term))
			throw new ConversionException();
		return new ExistenceError(term);
	}
	
}
