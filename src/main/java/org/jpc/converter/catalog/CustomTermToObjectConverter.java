package org.jpc.converter.catalog;

import java.lang.reflect.Type;

import org.jconverter.converter.ConverterEvaluator;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.FromTermConverterAdapter;
import org.jpc.term.Compound;
import org.jpc.term.Term;

public class CustomTermToObjectConverter<T> implements FromTermConverter<Compound, T> {

public static final String CUSTOM_TERM_FUNCTOR_NAME = "custom_jterm";
	
	@Override
	public T fromTerm(Compound term, Type targetType, Jpc jpc) {
		Term sourceTerm = term.arg(1);
		Term converterTerm = term.arg(2);
		FromTermConverter<Term, T> converter = jpc.fromTerm(converterTerm, FromTermConverter.class);
		ConverterEvaluator<Term, T> converterEvaluator = new ConverterEvaluator<>(sourceTerm, targetType, jpc);
		return (T)converterEvaluator.apply(FromTermConverterAdapter.forConverter(converter));
	}
	
}
