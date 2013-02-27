package org.jpc.converter.toterm.tolistterm;

import java.util.Iterator;

import org.jpc.Jpc;
import org.jpc.converter.toterm.ToTermConverter;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;

import com.google.common.base.Predicate;

public class IteratorToTermConverter extends ToTermConverter<Iterator<?>> {

	public IteratorToTermConverter() {
	}

	@Override
	public Term convert(Iterator<?> it, Jpc context) {
		ListTerm terms = new ListTerm();
		while(it.hasNext())
			terms.add(context.toTerm(it.next()));
		return terms.asTerm();
	}

}
