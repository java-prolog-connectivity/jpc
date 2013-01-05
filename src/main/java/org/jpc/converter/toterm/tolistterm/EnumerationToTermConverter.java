package org.jpc.converter.toterm.tolistterm;

import java.util.Collections;
import java.util.Enumeration;

import org.jpc.converter.toterm.DefaultObjectToTermConverter;
import org.jpc.converter.toterm.ObjectToTermConverter;
import org.jpc.term.Term;

public class EnumerationToTermConverter extends ObjectToListTermConverter<Enumeration> {
	
	public EnumerationToTermConverter() {
		this(new DefaultObjectToTermConverter());
	}
	
	public EnumerationToTermConverter(ObjectToTermConverter memberConverter) {
		super(memberConverter);
	}
	
	@Override
	public Term apply(Enumeration en) {
		return new IterableToTermConverter(getMemberConverter()).apply(Collections.list(en));
	}

}
