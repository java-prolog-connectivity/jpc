package org.jpc.converter.toterm.tolistterm;

import java.util.Arrays;

import org.jpc.converter.toterm.ObjectToTermConverter;
import org.jpc.term.Term;

public class ArrayToTermConverter extends ObjectToListTermConverter {

	public ArrayToTermConverter(ObjectToTermConverter memberConverter) {
		super(memberConverter);
	}
	
	@Override
	public Term apply(Object objects) {
		return new IterableToTermConverter(getMemberConverter()).apply(Arrays.asList((Object[])objects));
	}
	
}
