package org.jpc.converter.catalog.jterm;

import org.jpc.converter.ConverterManager;
import org.jpc.term.Compound;

public class ReferenceConverterManager extends ConverterManager<Object, Compound> {

	public ReferenceConverterManager() {
		register(new JTermConverter());
		register(new SerializedConverter());
	}

}
