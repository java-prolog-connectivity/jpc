package org.jpc.converter.catalog;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.term.JRef;

public class JRefToObjectConverter<T> implements FromTermConverter<JRef<T>, T> {

	@Override
	public T fromTerm(JRef<T> term, Type targetType, Jpc context) {
		return term.getReferent();
	}

}
