package org.jpc.converter.toterm.tolistterm;

import org.jpc.converter.toterm.DefaultObjectToTermConverter;
import org.jpc.converter.toterm.ObjectToTermConverter;
import org.jpc.term.Term;


public class IterableToTermConverter extends ObjectToListTermConverter<Iterable> {

	public IterableToTermConverter() {
		this(new DefaultObjectToTermConverter());
	}
	
	public IterableToTermConverter(ObjectToTermConverter memberConverter) {
		super(memberConverter);
	}
	
	@Override
	public Term apply(Iterable it) {
		return new IteratorToTermConverter(getMemberConverter()).apply(it.iterator());
	}

}
