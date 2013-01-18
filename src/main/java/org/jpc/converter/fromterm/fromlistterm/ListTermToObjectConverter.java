package org.jpc.converter.fromterm.fromlistterm;

import org.jpc.converter.fromterm.FromTermConverter;


public abstract class ListTermToObjectConverter<T> implements FromTermConverter<T> {

	private FromTermConverter memberConverter;
	
	public ListTermToObjectConverter(FromTermConverter memberConverter) {
		this.memberConverter = memberConverter;
	}

	public FromTermConverter getMemberConverter() {
		return memberConverter;
	}

}
