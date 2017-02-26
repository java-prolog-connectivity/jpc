package org.jpc.mapping.converter.catalog.refterm;

import static org.jconverter.converter.ConversionGoal.conversionGoal;

import org.jconverter.converter.DelegateConversionException;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.mapping.converter.FromTermConverter;
import org.jpc.term.Compound;

public class FromRefTermConverter<T> implements FromTermConverter<Compound, T> {

	@Override
	public T fromTerm(Compound term, TypeDomain target, Jpc context) {
		T resolved = (T) context.resolveRefTerm(term);
		if (resolved == null) {
			throw new DelegateConversionException(conversionGoal(term, target));
		}
		return resolved;
	}

}
