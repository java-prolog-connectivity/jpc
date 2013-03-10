package org.jpc.converter.catalog;

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
		if(!(term instanceof Atom || term instanceof NumberTerm))
			throw new JpcConversionException();
		String s = term.toString();
		if(s.length() > 1)
			throw new JpcConversionException();
		return s.charAt(0);
	}
	
}
