package org.jpc.converter.catalog.primitive;

import java.lang.reflect.Type;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.BidirectionalTermConverter;
import org.jpc.term.Atom;
import org.jpc.term.NumberTerm;
import org.jpc.term.Term;

public class CharacterConverter<T extends Term> implements BidirectionalTermConverter<Character, T> {

	@Override
	public T toTerm(Character c, Class<T> termClass, Jpc context) {
		return new StringConverter<T>().toTerm(c.toString(), termClass, context);
	}
	
	@Override
	public Character fromTerm(Term term, Type type, Jpc context) {
		String s;
		if(term instanceof Atom)
			s = ((Atom) term).getName();
		else if(term instanceof NumberTerm)
			s = term.toString();
		else
			throw new ConversionException();
		if(s.length() > 1)
			throw new ConversionException();
		return s.charAt(0);
	}
	
}
