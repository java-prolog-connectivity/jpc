package org.jpc.converter.catalog.jterm;

import org.jpc.converter.ConverterManager;
import org.jpc.term.Compound;

public class JTermConverterManager extends ConverterManager<Object, Compound> {

	public JTermConverterManager() {
		register(new JRefConverter());
		register(new SerializedConverter());
	}

}
