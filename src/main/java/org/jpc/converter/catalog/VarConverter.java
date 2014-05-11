package org.jpc.converter.catalog;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.term.AbstractVar;

public class VarConverter<T> implements FromTermConverter<AbstractVar, T> {
	
	@Override
	public T fromTerm(AbstractVar term, Type type, Jpc context) {
		return null;
	}
	
}
