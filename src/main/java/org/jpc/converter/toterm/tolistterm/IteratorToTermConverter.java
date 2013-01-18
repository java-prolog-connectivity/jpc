package org.jpc.converter.toterm.tolistterm;

import java.util.Iterator;

import org.jpc.converter.toterm.DefaultToTermConverter;
import org.jpc.converter.toterm.ToTermConverter;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;

public class IteratorToTermConverter extends ObjectToListTermConverter<Iterator> {

	public IteratorToTermConverter() {
		this(new DefaultToTermConverter());
	}
	
	public IteratorToTermConverter(ToTermConverter memberConverter) {
		super(memberConverter);
	}

	@Override
	public Term apply(Iterator it) {
		ListTerm terms = new ListTerm();
		while(it.hasNext())
			terms.add(getMemberConverter().apply(it.next()));
		return terms.asTerm();
	}

}
