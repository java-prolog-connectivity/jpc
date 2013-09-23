package org.jpc.converter.catalog;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.JpcConverter;
import org.jpc.term.Var;

public class NullConverter extends JpcConverter<Object, Var> {

	@Override
	public <T extends Var> T toTerm(Object o, Class<T> termClass, Jpc context) {
		if(o != null)
			throw new JpcConversionException();
		return (T) Var.ANONYMOUS_VAR;
	}
	
	@Override
	public Object fromTerm(Var term, Type type, Jpc context) {
		return null;
	}
	
}
