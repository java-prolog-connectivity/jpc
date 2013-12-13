package org.jpc.converter.catalog.error;

import static org.jpc.converter.catalog.error.IsoPrologErrorConverter.isIsoPrologError;

import java.lang.reflect.Type;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.error.ResourceError;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class ResourceErrorConverter implements FromTermConverter<Compound, ResourceError> {

	public static final String RESOURCE_ERROR_FUNCTOR = "resource_error";
	
	public static boolean isResourceError(Term term) {
		return isIsoPrologError(term) && term.arg(1).hasFunctor(RESOURCE_ERROR_FUNCTOR, 1);
	}
	
	@Override
	public ResourceError fromTerm(Compound term, Type type, Jpc context) {
		if(!isResourceError(term))
			throw new ConversionException();
		return new ResourceError(term);
	}
	
}
