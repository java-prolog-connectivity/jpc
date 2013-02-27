package org.jpc.converter.fromterm;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.term.Atom;
import org.jpc.term.NumberTerm;
import org.jpc.term.Term;

public class TermToCharConverter extends FromTermConverter<Character> {

	@Override
	public Character convert(Term term, Type type, Jpc context) {
		if(!(term instanceof Atom || term instanceof NumberTerm))
			throw new JpcConversionException();
		String s = term.toString();
		if(s.length() > 1)
			throw new JpcConversionException();
		return s.charAt(0);
	}

}
