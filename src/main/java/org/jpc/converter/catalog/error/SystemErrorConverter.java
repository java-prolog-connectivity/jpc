package org.jpc.converter.catalog.error;

import static org.jpc.converter.catalog.error.IsoPrologErrorConverter.isIsoPrologError;

import java.lang.reflect.Type;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.error.SystemError;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class SystemErrorConverter implements FromTermConverter<Compound, SystemError> {

	public static final String SYSTEM_ERROR_FUNCTOR = "system_error";
	
	public static boolean isSystemError(Term term) {
		return isIsoPrologError(term) && term.arg(1).hasFunctor(SYSTEM_ERROR_FUNCTOR, 0);
	}
	
	@Override
	public SystemError fromTerm(Compound term, Type type, Jpc context) {
		if(!isSystemError(term))
			throw new ConversionException();
		return new SystemError(term);
	}
	
}
