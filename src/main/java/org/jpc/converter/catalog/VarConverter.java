package org.jpc.converter.catalog;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.term.Var;

public class VarConverter<T> implements FromTermConverter<Var, T> {
	
	@Override
	public T fromTerm(Var term, Type type, Jpc context) {
		return null;
	}
	
}
