package org.jpc.mapping.converter;

import org.jconverter.JConverter;
import org.jconverter.converter.Converter;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.mapping.converter.FromTermConverter;
import org.jpc.term.Term;

public class FromTermConverterAdapter<T extends Term, V> implements Converter<T, V> {
    private final FromTermConverter<T,V> converter;

    public FromTermConverterAdapter(FromTermConverter<T,V> converter) {
        this.converter = converter;
    }

    @Override
    public V apply(T source, TypeDomain target, JConverter context) {
        return converter.fromTerm(source, target, (Jpc) context);
    }
}