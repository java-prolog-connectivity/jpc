package org.jpc.internal.jconverter;


import org.jconverter.JConverter;
import org.jconverter.converter.Converter;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Term;

public class ToTermConverterAdapter<T, V extends Term> implements Converter<T, V> {

    private final ToTermConverter<T,V> converter;

    public ToTermConverterAdapter(ToTermConverter<T,V> converter) {
        this.converter = converter;
    }

    @Override
    public V apply(T source, TypeDomain target, JConverter context) {
        return converter.toTerm(source, target, (Jpc) context);
    }

}
