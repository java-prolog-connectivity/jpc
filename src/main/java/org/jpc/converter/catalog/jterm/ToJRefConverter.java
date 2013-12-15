package org.jpc.converter.catalog.jterm;

import org.jpc.Jpc;
import org.jpc.converter.ToTermConverter;
import org.jpc.term.Compound;
import org.jpc.term.jterm.JTermUtil;

public class ToJRefConverter implements ToTermConverter<Object, Compound> {

	@Override
	public Compound toTerm(Object object, Class<Compound> termClass, Jpc context) {
		return JTermUtil.jRefTerm(object);
	}

}
