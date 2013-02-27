package org.jpc.converter.fromterm;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.JpcConverter;
import org.jpc.term.Term;

public abstract class FromTermConverter<T> extends JpcConverter<Term, T> {

	public abstract T convert(Term term, Type type, Jpc context);
	
	public boolean canConvert(Term term, Type toType) {
		return targetTypeIsAssignableFrom(toType);
	}

}
