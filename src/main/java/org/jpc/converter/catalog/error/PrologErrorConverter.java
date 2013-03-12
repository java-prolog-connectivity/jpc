package org.jpc.converter.catalog.error;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.JpcConverter;
import org.jpc.error.DomainError;
import org.jpc.error.PrologError;
import org.jpc.term.Compound;

public class PrologErrorConverter extends JpcConverter<PrologError, Compound> {

	@Override
	public PrologError fromTerm(Compound term, Type type, Jpc context) {
		if(!type.equals(PrologError.class))
			throw new JpcConversionException();
		return new PrologError(term);
	}
	
}
