package org.jpc.converter.catalog.primitive;

import java.lang.reflect.Type;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.BidirectionalTermConverter;
import org.jpc.term.Atom;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.NumberTerm;
import org.jpc.term.Term;

public class StringConverter<T extends Term> implements BidirectionalTermConverter<String, T> {

	@Override
	public T toTerm(String source, Class<T> termClass, Jpc context) {
		Term term = null;
		if(termClass.isAssignableFrom(Atom.class))
			term = new Atom(source);
		else if(NumberTerm.class.isAssignableFrom(termClass)) {
			if(termClass.equals(IntegerTerm.class))
				term = new IntegerTerm(Long.parseLong(source));
			else
				term = new FloatTerm(Long.parseLong(source));
		}
		if(term == null)
			throw new ConversionException();
		return (T) term;
	}

	@Override
	public String fromTerm(T term, Type type, Jpc context) {
		String termString;
		if(term instanceof Atom)
			termString = ((Atom) term).getName();
		else if(term instanceof NumberTerm)
			termString = term.toString();
		else
			throw new ConversionException();
		if(String.class.equals(type))
			return termString;
		else
			throw new ConversionException();
	}
	
}
