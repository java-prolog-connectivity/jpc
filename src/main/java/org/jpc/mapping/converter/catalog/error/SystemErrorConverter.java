package org.jpc.mapping.converter.catalog.error;

import static org.jconverter.converter.ConversionGoal.conversionGoal;
import static org.jpc.mapping.converter.catalog.error.IsoPrologErrorConverter.isIsoPrologError;

import org.jconverter.converter.DelegateConversionException;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.mapping.converter.FromTermConverter;
import org.jpc.error.SystemError;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class SystemErrorConverter implements FromTermConverter<Compound, SystemError> {

	public static final String SYSTEM_ERROR_FUNCTOR_NAME = "system_error";
	
	public static boolean isSystemError(Term term) {
		return isIsoPrologError(term) && term.arg(1).hasFunctor(SYSTEM_ERROR_FUNCTOR_NAME, 0);
	}
	
	@Override
	public SystemError fromTerm(Compound term, TypeDomain target, Jpc context) {
		if (!isSystemError(term)) {
			throw new DelegateConversionException(conversionGoal(term, target));
		}
		return new SystemError(term);
	}
	
}
