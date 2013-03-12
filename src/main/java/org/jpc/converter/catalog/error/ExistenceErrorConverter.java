package org.jpc.converter.catalog.error;

import static org.jpc.converter.catalog.error.IsoPrologErrorConverter.isIsoPrologError;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.JpcConverter;
import org.jpc.error.DomainError;
import org.jpc.error.ExistenceError;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class ExistenceErrorConverter extends JpcConverter<ExistenceError, Compound> {

	public static final String EXISTENCE_ERROR_FUNCTOR = "existence_error";
	
	public static boolean isExistenceError(Term term) {
		return isIsoPrologError(term) && term.arg(1).hasFunctor(EXISTENCE_ERROR_FUNCTOR, 2);
	}
	
	@Override
	public ExistenceError fromTerm(Compound term, Type type, Jpc context) {
		if(!isExistenceError(term) || !type.equals(ExistenceError.class))
			throw new JpcConversionException();
		return new ExistenceError(term);
	}
	
}
