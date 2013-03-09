package org.jpc.converter.toterm.tolistterm;

import java.util.Arrays;

import org.jpc.Jpc;
import org.jpc.converter.toterm.ToTermConverter;
import org.jpc.term.Term;

public class ArrayToTermConverter extends ToTermConverter<Object[]> {

	public ArrayToTermConverter() {
	}
	
	@Override
	public Term convert(Object[] objects, Jpc context) {
		return new IterableToTermConverter().convert(Arrays.asList(objects), context);
	}
	
}
