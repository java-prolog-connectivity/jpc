package org.jpc.converter.catalog.error;

import static org.jconverter.converter.ConversionGoal.conversionGoal;
import static org.jpc.converter.catalog.error.IsoPrologErrorConverter.isIsoPrologError;

import org.jconverter.converter.DelegateConversionException;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.error.ExistenceError;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class ExistenceErrorConverter implements FromTermConverter<Compound, ExistenceError> {

	public static final String EXISTENCE_ERROR_FUNCTOR_NAME = "existence_error";
	
	public static boolean isExistenceError(Term term) {
		return isIsoPrologError(term) && term.arg(1).hasFunctor(EXISTENCE_ERROR_FUNCTOR_NAME, 2);
	}
	
	@Override
	public ExistenceError fromTerm(Compound term, TypeDomain target, Jpc context) {
		if (!isExistenceError(term)) {
			throw new DelegateConversionException(conversionGoal(term, target));
		}
		return new ExistenceError(term);
	}
	
}
