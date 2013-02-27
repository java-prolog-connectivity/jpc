package org.jpc.converter.fromterm;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.term.Atom;
import org.jpc.term.NumberTerm;
import org.jpc.term.Term;

public class TermToStringConverter extends FromTermConverter<String> {

	@Override
	public String convert(Term term, Type type, Jpc context) {
		if(!String.class.equals(type))
			throw new JpcConversionException();
		if(term instanceof Atom || term instanceof NumberTerm)
			return term.toString();
		throw new JpcConversionException();
	}

}
