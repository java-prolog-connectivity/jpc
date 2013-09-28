package org.jpc.converter.catalog.primitive;

import org.jpc.converter.ConverterManager;
import org.jpc.term.Term;

public class PrimitiveConverterManager extends ConverterManager<Object, Term> {

	public PrimitiveConverterManager() {
		register(new CharacterConverter());
		register(new StringConverter());
		register(new BooleanConverter());
		register(new NumberConverter());
	}

}
