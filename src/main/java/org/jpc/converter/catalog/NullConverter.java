package org.jpc.converter.catalog;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.JpcConverter;
import org.jpc.term.Variable;

public class NullConverter extends JpcConverter<Object, Variable> {

	@Override
	public <T extends Variable> T toTerm(Object o, Class<T> termClass, Jpc context) {
		if(o != null)
			throw new JpcConversionException();
		return (T) Variable.ANONYMOUS_VAR;
	}
	
	@Override
	public Object fromTerm(Variable term, Type type, Jpc context) {
		return null;
	}
	
}
