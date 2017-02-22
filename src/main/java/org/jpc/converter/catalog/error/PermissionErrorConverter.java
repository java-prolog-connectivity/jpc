package org.jpc.converter.catalog.error;

import static org.jconverter.converter.ConversionGoal.conversionGoal;
import static org.jpc.converter.catalog.error.IsoPrologErrorConverter.isIsoPrologError;

import org.jconverter.converter.DelegateConversionException;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.error.PermissionError;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class PermissionErrorConverter implements FromTermConverter<Compound, PermissionError> {

	public static final String PERMISION_ERROR_FUNCTOR_NAME = "permision_error";
	
	public static boolean isPermissionError(Term term) {
		return isIsoPrologError(term) && term.arg(1).hasFunctor(PERMISION_ERROR_FUNCTOR_NAME, 3);
	}
	
	@Override
	public PermissionError fromTerm(Compound term, TypeDomain target, Jpc context) {
		if (!isPermissionError(term)) {
			throw new DelegateConversionException(conversionGoal(term, target));
		}
		return new PermissionError(term);
	}
	
}
