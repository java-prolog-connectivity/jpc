package org.jpc.converter.toterm.tolistterm;

import java.util.Collections;
import java.util.Enumeration;

import org.jpc.Jpc;
import org.jpc.converter.toterm.ToTermConverter;
import org.jpc.term.Term;

import com.google.common.base.Predicate;

public class EnumerationToTermConverter extends ToTermConverter<Enumeration<?>> {
	
	public EnumerationToTermConverter() {
	}

	@Override
	public Term convert(Enumeration<?> en, Jpc context) {
		return new IterableToTermConverter().convert(Collections.list(en), context);
	}

}
