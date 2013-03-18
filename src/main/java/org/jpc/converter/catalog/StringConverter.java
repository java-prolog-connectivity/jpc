package org.jpc.converter.catalog;

import java.lang.reflect.Type;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.JpcConverter;
import org.jpc.term.Atom;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.NumberTerm;
import org.jpc.term.Term;

public class StringConverter extends JpcConverter<Object, Term> {

	@Override
	public <T extends Term> T toTerm(Object object, Class<T> termClass, Jpc context) {
		String source;
		Term term = null;
		if(object instanceof String || object instanceof StringBuilder || object instanceof StringBuffer) {
			source = object.toString();
			if(termClass.isAssignableFrom(Atom.class))
				term = new Atom(source);
			else if(NumberTerm.class.isAssignableFrom(termClass)) {
				if(termClass.equals(IntegerTerm.class))
					term = new IntegerTerm(Long.parseLong(source));
				else
					term = new FloatTerm(Long.parseLong(source));
			}
		}
		if(term == null)
			throw new JpcConversionException();
		return (T) term;
	}

	@Override
	public String fromTerm(Term term, Type type, Jpc context) {
		if(!String.class.equals(type))
			throw new JpcConversionException();
		if(term instanceof Atom)
			return ((Atom) term).getName();
		else if(term instanceof NumberTerm)
			return term.toString();
		else
			throw new JpcConversionException();
	}
	
}
