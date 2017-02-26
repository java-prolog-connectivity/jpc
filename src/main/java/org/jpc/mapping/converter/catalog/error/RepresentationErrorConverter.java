package org.jpc.mapping.converter.catalog.error;

import static org.jconverter.converter.ConversionGoal.conversionGoal;
import static org.jpc.mapping.converter.catalog.error.IsoPrologErrorConverter.isIsoPrologError;

import org.jconverter.converter.DelegateConversionException;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.mapping.converter.FromTermConverter;
import org.jpc.error.RepresentationError;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class RepresentationErrorConverter implements FromTermConverter<Compound, RepresentationError> {

	public static final String REPRESENTATION_ERROR_FUNCTOR_NAME = "representation_error";
	
	public static boolean isRepresentationError(Term term) {
		return isIsoPrologError(term) && term.arg(1).hasFunctor(REPRESENTATION_ERROR_FUNCTOR_NAME, 1);
	}
	
	@Override
	public RepresentationError fromTerm(Compound term, TypeDomain target, Jpc context) {
		if(!isRepresentationError(term)) {
			throw new DelegateConversionException(conversionGoal(term, target));
		}
		return new RepresentationError(term);
	}
	
}
