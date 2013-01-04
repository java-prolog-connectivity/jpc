package org.jpc.converter.toterm.tolistterm;

import org.jpc.converter.fromterm.TermToObjectConverter;
import org.jpc.converter.toterm.ObjectToTermConverter;


public abstract class ObjectToListTermConverter<O> implements ObjectToTermConverter<O> {

	private ObjectToTermConverter memberConverter;
	
	public ObjectToListTermConverter(ObjectToTermConverter memberConverter) {
		this.memberConverter = memberConverter;
	}

	public ObjectToTermConverter getMemberConverter() {
		return memberConverter;
	}

}
