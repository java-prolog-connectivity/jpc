package org.jpc.converter.toterm.tolistterm;

import org.jpc.Jpc;
import org.jpc.converter.toterm.ToTermConverter;
import org.jpc.term.Term;

import com.google.common.base.Predicate;


public class IterableToTermConverter extends ToTermConverter<Iterable<?>> {

	public IterableToTermConverter() {
	}
	
	@Override
	public Term convert(Iterable<?> it, Jpc context) {
		return new IteratorToTermConverter().convert(it.iterator(), context);
	}

}
