package org.jpc.converter.catalog;

import static org.jconverter.converter.ConversionGoal.conversionGoal;

import org.jconverter.converter.ConverterEvaluator;
import org.jconverter.converter.InterTypeConverterEvaluator;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.internal.jconverter.Adapters;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class CustomTermToObjectConverter<T> implements FromTermConverter<Compound, T> {

public static final String CUSTOM_TERM_FUNCTOR_NAME = "jconvertable";
	
	@Override
	public T fromTerm(Compound term, TypeDomain target, Jpc jpc) {
		Term sourceTerm = term.arg(1);
		Term converterTerm = term.arg(2);
		FromTermConverter<Term, T> converter = jpc.fromTerm(converterTerm, FromTermConverter.class);
		ConverterEvaluator<Term, T> converterEvaluator = new InterTypeConverterEvaluator<>(conversionGoal(sourceTerm, target), jpc);
		return (T) converterEvaluator.apply(Adapters.asConversionFunction(converter));
	}
	
}
