package org.jpc.converter.catalog.error;

import static org.jconverter.converter.ConversionGoal.conversionGoal;
import static org.jpc.converter.catalog.error.IsoPrologErrorConverter.isIsoPrologError;

import org.jconverter.converter.DelegateConversionException;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.error.TypeError;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class TypeErrorConverter implements FromTermConverter<Compound, TypeError> {

	public static final String TYPE_ERROR_FUNCTOR_NAME = "type_error";
	
	public static boolean isTypeError(Term term) {
		return isIsoPrologError(term) && term.arg(1).hasFunctor(TYPE_ERROR_FUNCTOR_NAME, 2);
	}
	
	@Override
	public TypeError fromTerm(Compound term, TypeDomain target, Jpc context) {
		if(!isTypeError(term)) {
			throw new DelegateConversionException(conversionGoal(term, target));
		}
		return new TypeError(term);
	}
	
}
