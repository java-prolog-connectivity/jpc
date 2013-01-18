package org.jpc.converter.fromterm.fromlistterm;

import java.util.Collections;
import java.util.Enumeration;

import org.jpc.converter.fromterm.DefaultFromTermConverter;
import org.jpc.converter.fromterm.FromTermConverter;
import org.jpc.term.Term;

public class ListTermToEnumerationConverter<T> extends ListTermToObjectConverter<Enumeration<T>> {

	public ListTermToEnumerationConverter() {
		this((FromTermConverter<T>) new DefaultFromTermConverter());
	}
	
	public ListTermToEnumerationConverter(FromTermConverter<T> memberConverter) {
		super(memberConverter);
	}

	@Override
	public Enumeration<T> apply(Term term) {
		return Collections.enumeration(new ListTermToListConverter<T>(getMemberConverter()).apply(term));
	}
	
}
