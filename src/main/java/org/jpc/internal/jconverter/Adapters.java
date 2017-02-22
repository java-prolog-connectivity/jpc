package org.jpc.internal.jconverter;


import static org.jconverter.converter.ConversionDomains.conversionTypes;
import static org.jconverter.converter.TypeDomain.typeDomain;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

import org.jconverter.converter.Converter;
import org.jconverter.converter.DelegateConversionException;
import org.jconverter.converter.TypedConverter;
import org.typetools.typewrapper.TypeWrapper;
import org.jgum.strategy.ChainOfResponsibility;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Term;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Adapters {

    private final static Logger logger = LoggerFactory.getLogger(Adapters.class);

    public static <T extends Term, V> ChainOfResponsibility<Converter<T, V>, V> asChainOfFromTermConverters(List<FromTermConverter<T,V>> converters) {
        List<Converter<T,V>> typedConverters = new ArrayList<>();
        for (FromTermConverter<T,V> converter : converters) {
            typedConverters.add(asTypedConverter(converter));
        }
        ChainOfResponsibility<Converter<T,V>,V> chain = new ChainOfResponsibility<>(typedConverters, DelegateConversionException.class);
        return chain;
    }

    public static <T extends Term, V> TypedConverter<T, V> asTypedConverter(FromTermConverter<T, V> converter) {
        Type sourceType;
        Type targetType;
        TypeWrapper converterTypeWrapper = TypeWrapper.wrap(converter.getClass()).as(FromTermConverter.class);
        if (converterTypeWrapper.hasActualTypeArguments()) {
            sourceType = converterTypeWrapper.getActualTypeArguments()[0];
            targetType = converterTypeWrapper.getActualTypeArguments()[1];
            return TypedConverter.forConverter(new FromTermConverterAdapter<>(converter), conversionTypes(typeDomain(sourceType), typeDomain(targetType)));
        } else {
/*			logger.warn("Converter does not specify parameter types. Source type will be considered Term.class and target type Object class.");
			sourceType = Term.class;
			targetType = Object.class;
			return TypedConverter.asTypedConverter(new FromTermConverterAdapter<>(converter), sourceType, targetType);*/
            return TypedConverter.forConverter(new FromTermConverterAdapter<>(converter));
        }
    }

    public static <T,V extends Term> ChainOfResponsibility<Converter<T,V>, V> asChainOfToTermConverters(List<ToTermConverter<T,V>> converters) {
        List<Converter<T,V>> typedConverters = new ArrayList<>();
        for (ToTermConverter<T,V> converter : converters) {
            typedConverters.add(asTypedConverter(converter));
        }
        ChainOfResponsibility<Converter<T,V>,V> chain = new ChainOfResponsibility<>(typedConverters);
        return chain;
    }

    public static <T,V extends Term> TypedConverter<T, V> asTypedConverter(ToTermConverter<T,V> converter) {
        Type sourceType;
        Type targetType;
        TypeWrapper converterTypeWrapper = TypeWrapper.wrap(converter.getClass()).as(ToTermConverter.class);
        if(converterTypeWrapper.hasActualTypeArguments()) {
            sourceType = converterTypeWrapper.getActualTypeArguments()[0];
            targetType = converterTypeWrapper.getActualTypeArguments()[1];
            return TypedConverter.forConverter(new ToTermConverterAdapter<>(converter), conversionTypes(typeDomain(sourceType), typeDomain(targetType)));
        } else {
/*			logger.warn("Converter does not specify parameter types. Source type will be considered Object.class and target type Term class.");
			sourceType = Object.class;
			targetType = Term.class;
			return TypedConverter.asTypedConverter(new ToTermConverterAdapter<>(converter));*/
            return TypedConverter.forConverter(new ToTermConverterAdapter<>(converter));
        }
    }

}
