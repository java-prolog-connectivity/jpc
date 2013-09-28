package org.jpc.converter.catalog.primitive;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.JpcConverter;
import org.jpc.term.Atom;
import org.jpc.term.NumberTerm;
import org.jpc.term.Term;

public class CharacterConverter extends JpcConverter<Character, Term> {

	@Override
	public <T extends Term> T toTerm(Character c, Class<T> termClass, Jpc context) {
		return new StringConverter().toTerm(c.toString(), termClass, context);
	}
	
	@Override
	public Character fromTerm(Term term, Type type, Jpc context) {
		String s;
		if(term instanceof Atom)
			s = ((Atom) term).getName();
		else if(term instanceof NumberTerm)
			s = term.toString();
		else
			throw new JpcConversionException();
		if(s.length() > 1)
			throw new JpcConversionException();
		return s.charAt(0);
	}
	
}
