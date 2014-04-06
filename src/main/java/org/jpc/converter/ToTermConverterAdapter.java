package org.jpc.converter;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

import org.jconverter.JConverter;
import org.jconverter.converter.Converter;
import org.jconverter.converter.TypedConverter;
import org.jgum.strategy.ChainOfResponsibility;
import org.jpc.Jpc;
import org.jpc.term.Term;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ToTermConverterAdapter<T,V extends Term> extends TypedConverter<T, V>{

	private final static Logger logger = LoggerFactory.getLogger(ToTermConverterAdapter.class);
	
	public static <T,V extends Term> ChainOfResponsibility<Converter<T,V>, V> chainConverters(List<ToTermConverter<T,V>> converters) {
		List<Converter<T,V>> typedConverters = new ArrayList<>();
		for(ToTermConverter<T,V> converter : converters)
			typedConverters.add(forConverter(converter));
		ChainOfResponsibility<Converter<T,V>,V> chain = new ChainOfResponsibility<>(typedConverters);
		return chain;
	}
	
	public static <T,V extends Term> ToTermConverterAdapter forConverter(ToTermConverter<T,V> converter) {
		Type sourceType;
		Type returnType;
		TypeWrapper converterTypeWrapper = TypeWrapper.wrap(converter.getClass()).as(ToTermConverter.class);
		if(converterTypeWrapper.hasActualTypeArguments()) {
			sourceType = converterTypeWrapper.getActualTypeArguments()[0];
			returnType = converterTypeWrapper.getActualTypeArguments()[1];
		} else {
			logger.warn("Converter does not specify parameter types. Source type will be considered Object.class and target type Term class.");
			sourceType = Object.class;
			returnType = Term.class;
		}
		return new ToTermConverterAdapter<>(converter, sourceType, returnType);
	}
	
	private final ToTermConverter<T,V> converter;

	public ToTermConverterAdapter(ToTermConverter<T,V> converter, Type sourceType, Type returnType) {
		super(sourceType, returnType);
		this.converter = converter;
	}
	
	@Override
	public V apply(T source, Type targetType, JConverter context) {
		return (V)converter.toTerm(source, (Class)targetType, (Jpc)context);
	}

}
