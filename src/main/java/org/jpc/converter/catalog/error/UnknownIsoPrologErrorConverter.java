package org.jpc.converter.catalog.error;

import java.lang.reflect.Type;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.error.UnknownIsoPrologError;
import org.jpc.term.Compound;

public class UnknownIsoPrologErrorConverter implements FromTermConverter<Compound, UnknownIsoPrologError> {

	@Override
	public UnknownIsoPrologError fromTerm(Compound term, Type type, Jpc context) {
		if(!IsoPrologErrorConverter.isIsoPrologError(term))
			throw new ConversionException();
		return new UnknownIsoPrologError(term);
	}
	
}
