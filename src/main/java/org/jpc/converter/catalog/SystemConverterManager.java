package org.jpc.converter.catalog;

import org.jpc.converter.ConverterManager;
import org.jpc.converter.catalog.jterm.JTermConverter;
import org.jpc.converter.catalog.jterm.SerializedConverter;
import org.jpc.term.Term;

public class SystemConverterManager extends ConverterManager<Object, Term> {

	public SystemConverterManager() {
		register(new JTermConverter());
		register(new SerializedConverter());
		register(new TermConvertableConverter());
	}
	
}
