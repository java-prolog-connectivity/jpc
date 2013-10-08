package org.jpc.converter.catalog.list;

import java.util.Iterator;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.JpcConverter;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;

public class IteratorConverter<E> extends JpcConverter<Iterator<E>, Term> {

	@Override
	public <T extends Term> T toTerm(Iterator<E> it, Class<T> termClass, Jpc context) {
		if(!Term.class.isAssignableFrom(termClass))
			throw new JpcConversionException();
		ListTerm terms = new ListTerm();
		while(it.hasNext())
			terms.add(context.toTerm(it.next()));
		return (T) terms.asTerm();
	}

}
