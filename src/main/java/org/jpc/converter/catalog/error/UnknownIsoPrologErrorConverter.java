package org.jpc.converter.catalog.error;

import static org.jconverter.converter.ConversionGoal.conversionGoal;

import org.jconverter.converter.DelegateConversionException;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.error.UnknownIsoPrologError;
import org.jpc.term.Compound;

public class UnknownIsoPrologErrorConverter implements FromTermConverter<Compound, UnknownIsoPrologError> {

	@Override
	public UnknownIsoPrologError fromTerm(Compound term, TypeDomain target, Jpc context) {
		if (!IsoPrologErrorConverter.isIsoPrologError(term)) {
			throw new DelegateConversionException(conversionGoal(term, target));
		}
		return new UnknownIsoPrologError(term);
	}
	
}
