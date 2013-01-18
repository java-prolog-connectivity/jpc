package org.jpc.converter.toterm.tolistterm;

import org.jpc.converter.fromterm.FromTermConverter;
import org.jpc.converter.toterm.ToTermConverter;


public abstract class ObjectToListTermConverter<O> implements ToTermConverter<O> {

	private ToTermConverter memberConverter;
	
	public ObjectToListTermConverter(ToTermConverter memberConverter) {
		this.memberConverter = memberConverter;
	}

	public ToTermConverter getMemberConverter() {
		return memberConverter;
	}

}
