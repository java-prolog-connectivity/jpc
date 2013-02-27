package org.jpc.converter.fromterm;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.term.Atom;
import org.jpc.term.Term;

public class TermToBooleanConverter extends FromTermConverter<Boolean> {

	@Override
	public Boolean convert(Term term, Type type, Jpc context) {
		if(!Boolean.class.equals(type))
			throw new JpcConversionException();
		try {
			Atom atom = (Atom) term;
			return Boolean.parseBoolean(atom.toString());
		} catch(Exception e) {
			throw new JpcConversionException();
		}
	}

}
