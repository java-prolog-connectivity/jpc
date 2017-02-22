package org.jpc.converter.catalog.error;

import static org.jconverter.converter.ConversionGoal.conversionGoal;
import static org.jpc.converter.catalog.error.IsoPrologErrorConverter.isIsoPrologError;

import org.jconverter.converter.DelegateConversionException;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.error.DomainError;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class DomainErrorConverter implements FromTermConverter<Compound, DomainError> {

	public static final String DOMAIN_ERROR_FUNCTOR_NAME = "domain_error";
	
	public static boolean isDomainError(Term term) {
		return isIsoPrologError(term) && term.arg(1).hasFunctor(DOMAIN_ERROR_FUNCTOR_NAME, 2);
	}
	
	@Override
	public DomainError fromTerm(Compound term, TypeDomain target, Jpc context) {
		if (!isDomainError(term)) {
			throw new DelegateConversionException(conversionGoal(term, target));
		}
		return new DomainError(term);
	}

}
