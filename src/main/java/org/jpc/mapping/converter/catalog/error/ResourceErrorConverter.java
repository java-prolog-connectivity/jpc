package org.jpc.mapping.converter.catalog.error;

import static org.jconverter.converter.ConversionGoal.conversionGoal;
import static org.jpc.mapping.converter.catalog.error.IsoPrologErrorConverter.isIsoPrologError;

import org.jconverter.converter.DelegateConversionException;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.mapping.converter.FromTermConverter;
import org.jpc.error.ResourceError;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class ResourceErrorConverter implements FromTermConverter<Compound, ResourceError> {

	public static final String RESOURCE_ERROR_FUNCTOR_NAME = "resource_error";
	
	public static boolean isResourceError(Term term) {
		return isIsoPrologError(term) && term.arg(1).hasFunctor(RESOURCE_ERROR_FUNCTOR_NAME, 1);
	}
	
	@Override
	public ResourceError fromTerm(Compound term, TypeDomain target, Jpc context) {
		if(!isResourceError(term)) {
			throw new DelegateConversionException(conversionGoal(term, target));
		}
		return new ResourceError(term);
	}
	
}
