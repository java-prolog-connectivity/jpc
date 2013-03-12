package org.jpc.converter.catalog.error;

import static org.jpc.converter.catalog.error.IsoPrologErrorConverter.isIsoPrologError;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.JpcConverter;
import org.jpc.error.DomainError;
import org.jpc.error.PermissionError;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class PermissionErrorConverter extends JpcConverter<PermissionError, Compound> {

	public static final String PERMISION_ERROR_FUNCTOR = "permision_error";
	
	public static boolean isPermissionError(Term term) {
		return isIsoPrologError(term) && term.arg(1).hasFunctor(PERMISION_ERROR_FUNCTOR, 3);
	}
	
	@Override
	public PermissionError fromTerm(Compound term, Type type, Jpc context) {
		if(!isPermissionError(term) || !type.equals(PermissionError.class))
			throw new JpcConversionException();
		return new PermissionError(term);
	}
	
}
