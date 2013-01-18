package org.jpc.converter.toterm.tolistterm;

import org.jpc.converter.toterm.DefaultToTermConverter;
import org.jpc.converter.toterm.ToTermConverter;
import org.jpc.term.Term;


public class IterableToTermConverter extends ObjectToListTermConverter<Iterable> {

	public IterableToTermConverter() {
		this(new DefaultToTermConverter());
	}
	
	public IterableToTermConverter(ToTermConverter memberConverter) {
		super(memberConverter);
	}
	
	@Override
	public Term apply(Iterable it) {
		return new IteratorToTermConverter(getMemberConverter()).apply(it.iterator());
	}

}
