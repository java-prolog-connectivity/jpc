package org.jpc.converter;

import org.jpc.converter.catalog.datetime.DateTimeConverterManager;
import org.jpc.converter.catalog.error.IsoPrologErrorConverterManager;
import org.jpc.converter.catalog.list.ListTermConverterManager;
import org.jpc.converter.catalog.map.MapEntryConverterManager;
import org.jpc.converter.catalog.primitive.PrimitiveConverterManager;
import org.jpc.term.Term;

public class DefaultJpcConverterManager extends ConverterManager<Object, Term> {

	public DefaultJpcConverterManager() {
		register(new IsoPrologErrorConverterManager());
		register(new ListTermConverterManager());
		register(new MapEntryConverterManager());
		register(new DateTimeConverterManager());
		register(new PrimitiveConverterManager());
	}

}
