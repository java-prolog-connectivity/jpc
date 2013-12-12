package org.jpc.converter;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;
import org.jconverter.JConverter;
import org.jconverter.converter.Converter;
import org.jconverter.converter.TypedConverter;
import org.jgum.strategy.ChainOfResponsibility;
import org.jpc.Jpc;
import org.jpc.term.Term;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;

public class FromTermConverterAdapter<T extends Term, V> extends TypedConverter<T, V> {

	private final static Logger logger = Logger.getLogger(FromTermConverterAdapter.class);
	
	public static <T extends Term,V> ChainOfResponsibility<Converter<T,V>, V> chainConverters(List<FromTermConverter<T,V>> converters) {
		List<Converter<T,V>> typedConverters = new ArrayList<>();
		for(FromTermConverter<T,V> converter : converters)
			typedConverters.add(forConverter(converter));
		ChainOfResponsibility<Converter<T,V>,V> chain = new ChainOfResponsibility<>(typedConverters);
		return chain;
	}

	public static <T extends Term,V> FromTermConverterAdapter forConverter(FromTermConverter<T,V> converter) {
		Type sourceType;
		Type returnType;
		TypeWrapper converterTypeWrapper = TypeWrapper.wrap(converter.getClass()).as(FromTermConverter.class);
		if(converterTypeWrapper.hasActualTypeArguments()) {
			sourceType = converterTypeWrapper.getActualTypeArguments()[0];
			returnType = converterTypeWrapper.getActualTypeArguments()[1];
		} else {
			logger.warn("Converter does not specify parameter types. Source type will be considered Term.class and target type Object class.");
			sourceType = Term.class;
			returnType = Object.class;
		}
		return new FromTermConverterAdapter<>(converter, sourceType, returnType);
	}
	
	private final FromTermConverter<T,V> converter;

	public FromTermConverterAdapter(FromTermConverter<T,V> converter, Type sourceType, Type returnType) {
		super(sourceType, returnType);
		this.converter = converter;
	}

	@Override
	public V apply(T source, Type targetType, JConverter context) {
		return converter.fromTerm(source, targetType, (Jpc)context);
	}

}
