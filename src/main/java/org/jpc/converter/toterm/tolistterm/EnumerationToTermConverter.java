package org.jpc.converter.toterm.tolistterm;

import java.util.Collections;
import java.util.Enumeration;

import org.jpc.converter.toterm.DefaultToTermConverter;
import org.jpc.converter.toterm.ToTermConverter;
import org.jpc.term.Term;

public class EnumerationToTermConverter extends ObjectToListTermConverter<Enumeration> {
	
	public EnumerationToTermConverter() {
		this(new DefaultToTermConverter());
	}
	
	public EnumerationToTermConverter(ToTermConverter memberConverter) {
		super(memberConverter);
	}
	
	@Override
	public Term apply(Enumeration en) {
		return new IterableToTermConverter(getMemberConverter()).apply(Collections.list(en));
	}

}
