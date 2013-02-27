package org.jpc.converter.fromterm;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.term.Term;
import org.jpc.term.Variable;

public class VariableTermToObjectConverter extends FromTermConverter<Object> {

	@Override
	public Object convert(Term term, Type type, Jpc context) {
		if(!(term instanceof Variable))
			throw new JpcConversionException();
		return null;
	}

}
