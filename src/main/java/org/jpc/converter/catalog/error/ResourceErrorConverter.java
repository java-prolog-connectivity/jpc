package org.jpc.converter.catalog.error;

import static org.jpc.converter.catalog.error.IsoPrologErrorConverter.isIsoPrologError;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.JpcConverter;
import org.jpc.error.DomainError;
import org.jpc.error.ResourceError;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class ResourceErrorConverter extends JpcConverter<ResourceError, Compound> {

	public static final String RESOURCE_ERROR_FUNCTOR = "resource_error";
	
	public static boolean isResourceError(Term term) {
		return isIsoPrologError(term) && term.arg(1).hasFunctor(RESOURCE_ERROR_FUNCTOR, 1);
	}
	
	@Override
	public ResourceError fromTerm(Compound term, Type type, Jpc context) {
		if(!isResourceError(term) || !type.equals(ResourceError.class))
			throw new JpcConversionException();
		return new ResourceError(term);
	}
	
}
