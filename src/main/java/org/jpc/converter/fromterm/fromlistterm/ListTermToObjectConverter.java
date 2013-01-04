package org.jpc.converter.fromterm.fromlistterm;

import org.jpc.converter.fromterm.TermToObjectConverter;


public abstract class ListTermToObjectConverter<T> implements TermToObjectConverter<T> {

	private TermToObjectConverter memberConverter;
	
	public ListTermToObjectConverter(TermToObjectConverter memberConverter) {
		this.memberConverter = memberConverter;
	}

	public TermToObjectConverter getMemberConverter() {
		return memberConverter;
	}

}
