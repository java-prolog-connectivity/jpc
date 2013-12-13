package org.jpc.converter.catalog.error;

import static org.jpc.converter.catalog.error.IsoPrologErrorConverter.isIsoPrologError;

import java.lang.reflect.Type;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.error.PermissionError;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class PermissionErrorConverter implements FromTermConverter<Compound, PermissionError> {

	public static final String PERMISION_ERROR_FUNCTOR = "permision_error";
	
	public static boolean isPermissionError(Term term) {
		return isIsoPrologError(term) && term.arg(1).hasFunctor(PERMISION_ERROR_FUNCTOR, 3);
	}
	
	@Override
	public PermissionError fromTerm(Compound term, Type type, Jpc context) {
		if(!isPermissionError(term))
			throw new ConversionException();
		return new PermissionError(term);
	}
	
}
