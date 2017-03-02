package org.jpc.mapping.converter.catalog;

import static org.jconverter.converter.ConversionGoal.conversionGoal;
import static org.jconverter.converter.TypeDomain.typeDomain;

import org.jconverter.converter.ConversionError;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.mapping.converter.FromTermConverter;
import org.jpc.term.AbstractVar;

public class VarConverter<T> implements FromTermConverter<AbstractVar, T> {
	
	@Override
	public T fromTerm(AbstractVar term, TypeDomain target, Jpc context) {
		if (typeDomain(term.getClass()).isSubsetOf(target)) {
			return (T) term;
		}
		throw new ConversionError(conversionGoal(term, target));
	}
	
}
