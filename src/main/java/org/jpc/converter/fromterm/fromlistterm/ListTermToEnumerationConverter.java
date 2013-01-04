package org.jpc.converter.fromterm.fromlistterm;

import java.util.Collections;
import java.util.Enumeration;

import org.jpc.converter.fromterm.TermToObjectConverter;
import org.jpc.term.Term;

public class ListTermToEnumerationConverter extends ListTermToObjectConverter<Enumeration> {

	public ListTermToEnumerationConverter(TermToObjectConverter memberConverter) {
		super(memberConverter);
	}

	@Override
	public Enumeration apply(Term term) {
		return Collections.enumeration(new ListTermToCollectionConverter(getMemberConverter()).apply(term));
	}
	
}
