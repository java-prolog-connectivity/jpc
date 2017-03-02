package org.jpc.mapping.converter;


import static org.jconverter.converter.TypeDomain.typeDomain;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

import org.jcategory.strategy.ChainOfResponsibility;
import org.jconverter.JConverter;
import org.jconverter.converter.ConversionFunction;
import org.jconverter.converter.Converter;
import org.jconverter.converter.DelegateConversionException;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.term.Term;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.typeutils.typewrapper.TypeWrapper;

public class Adapters {

    private final static Logger logger = LoggerFactory.getLogger(Adapters.class);

    public static <T extends Term, V> ChainOfResponsibility<Converter<T, V>, V> asChainOfFromTermConverters(List<FromTermConverter<T,V>> converters) {
        List<Converter<T,V>> typedConverters = new ArrayList<>();
        for (FromTermConverter<T,V> converter : converters) {
            typedConverters.add(asConversionFunction(converter));
        }
        ChainOfResponsibility<Converter<T,V>,V> chain = new ChainOfResponsibility<>(typedConverters, DelegateConversionException.class);
        return chain;
    }

    public static <T extends Term, V> ConversionFunction<T, V> asConversionFunction(FromTermConverter<T, V> converter) {
        Type sourceType;
        Type targetType;
        TypeWrapper converterTypeWrapper = TypeWrapper.wrap(converter.getClass()).as(FromTermConverter.class);
        if (converterTypeWrapper.hasActualTypeArguments()) {
            sourceType = converterTypeWrapper.getActualTypeArguments()[0];
            targetType = converterTypeWrapper.getActualTypeArguments()[1];
            return ConversionFunction.forConverter(new FromTermConverterAdapter<>(converter), typeDomain(sourceType), typeDomain(targetType));
        } else {
/*			logger.warn("Converter does not specify parameter types. Source type will be considered Term.class and target type Object class.");
			sourceType = Term.class;
			targetType = Object.class;
			return TypedConverter.asConversionFunction(new FromTermConverterAdapter<>(converter), sourceType, targetType);*/
            return ConversionFunction.forConverter(new FromTermConverterAdapter<>(converter));
        }
    }

    public static <T,V extends Term> ChainOfResponsibility<Converter<T,V>, V> asChainOfToTermConverters(List<ToTermConverter<T,V>> converters) {
        List<Converter<T,V>> typedConverters = new ArrayList<>();
        for (ToTermConverter<T,V> converter : converters) {
            typedConverters.add(asConversionFunction(converter));
        }
        ChainOfResponsibility<Converter<T,V>,V> chain = new ChainOfResponsibility<>(typedConverters);
        return chain;
    }

    public static <T,V extends Term> ConversionFunction<T, V> asConversionFunction(ToTermConverter<T,V> converter) {
        Type sourceType;
        Type targetType;
        TypeWrapper converterTypeWrapper = TypeWrapper.wrap(converter.getClass()).as(ToTermConverter.class);
        if (converterTypeWrapper.hasActualTypeArguments()) {
            sourceType = converterTypeWrapper.getActualTypeArguments()[0];
            targetType = converterTypeWrapper.getActualTypeArguments()[1];
            return ConversionFunction.forConverter(new ToTermConverterAdapter<>(converter), typeDomain(sourceType), typeDomain(targetType));
        } else {
/*			logger.warn("Converter does not specify parameter types. Source type will be considered Object.class and target type Term class.");
			sourceType = Object.class;
			targetType = Term.class;
			return TypedConverter.asConversionFunction(new ToTermConverterAdapter<>(converter));*/
            return ConversionFunction.forConverter(new ToTermConverterAdapter<>(converter));
        }
    }



    public static class FromTermConverterAdapter<T extends Term, V> implements Converter<T, V> {
        private final FromTermConverter<T,V> converter;

        public FromTermConverterAdapter(FromTermConverter<T,V> converter) {
            this.converter = converter;
        }

        @Override
        public V apply(T source, TypeDomain target, JConverter context) {
            return converter.fromTerm(source, target, (Jpc) context);
        }
    }



    public static class ToTermConverterAdapter<T, V extends Term> implements Converter<T, V> {

        private final ToTermConverter<T,V> converter;

        public ToTermConverterAdapter(ToTermConverter<T,V> converter) {
            this.converter = converter;
        }

        @Override
        public V apply(T source, TypeDomain target, JConverter context) {
            return converter.toTerm(source, target, (Jpc) context);
        }

    }


}
