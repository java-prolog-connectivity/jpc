package org.jpc.converter.toterm;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.term.Atom;
import org.jpc.term.Term;

public class StringToTermConverter extends ToTermConverter<Object> {

	@Override
	public Term convert(Object source, Jpc context) {
		if(source instanceof String || source instanceof StringBuilder || source instanceof StringBuffer || source instanceof Character)
			return new Atom(source.toString());
		else
			throw new JpcConversionException();
	}

}
