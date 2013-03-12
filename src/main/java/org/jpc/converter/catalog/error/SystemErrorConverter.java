package org.jpc.converter.catalog.error;

import static org.jpc.converter.catalog.error.IsoPrologErrorConverter.isIsoPrologError;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.JpcConverter;
import org.jpc.error.DomainError;
import org.jpc.error.SystemError;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class SystemErrorConverter extends JpcConverter<SystemError, Compound> {

	public static final String SYSTEM_ERROR_FUNCTOR = "system_error";
	
	public static boolean isSystemError(Term term) {
		return isIsoPrologError(term) && term.arg(1).hasFunctor(SYSTEM_ERROR_FUNCTOR, 0);
	}
	
	@Override
	public SystemError fromTerm(Compound term, Type type, Jpc context) {
		if(!isSystemError(term) || !type.equals(SystemError.class))
			throw new JpcConversionException();
		return new SystemError(term);
	}
	
}
