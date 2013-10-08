package org.jpc.converter.catalog.list;

import org.jpc.Jpc;
import org.jpc.converter.JpcConverter;
import org.jpc.term.Term;


public class IterableConverter<E> extends JpcConverter<Iterable<E>, Term> {

	@Override
	public <T extends Term> T  toTerm(Iterable<E> it, Class<T> termClass, Jpc context) {
		return new IteratorConverter<E>().toTerm(it.iterator(), termClass, context);
	}

}
