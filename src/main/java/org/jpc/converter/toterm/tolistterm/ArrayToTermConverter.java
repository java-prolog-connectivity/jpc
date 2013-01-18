package org.jpc.converter.toterm.tolistterm;

import java.util.Arrays;

import org.jpc.converter.toterm.DefaultToTermConverter;
import org.jpc.converter.toterm.ToTermConverter;
import org.jpc.term.Term;

public class ArrayToTermConverter extends ObjectToListTermConverter {

	public ArrayToTermConverter() {
		this(new DefaultToTermConverter());
	}
	
	public ArrayToTermConverter(ToTermConverter memberConverter) {
		super(memberConverter);
	}
	
	@Override
	public Term apply(Object objects) {
		return new IterableToTermConverter(getMemberConverter()).apply(Arrays.asList((Object[])objects));
	}
	
}
