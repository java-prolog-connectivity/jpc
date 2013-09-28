package org.jpc.converter;

import org.jpc.converter.catalog.BooleanConverter;
import org.jpc.converter.catalog.CalendarConverter;
import org.jpc.converter.catalog.CharacterConverter;
import org.jpc.converter.catalog.JRefConverter;
import org.jpc.converter.catalog.NumberConverter;
import org.jpc.converter.catalog.SerializedConverter;
import org.jpc.converter.catalog.StringConverter;
import org.jpc.converter.catalog.XmlGregorianCalendarConverter;
import org.jpc.converter.catalog.error.IsoPrologErrorConverterManager;
import org.jpc.converter.catalog.listterm.ArrayConverter;
import org.jpc.converter.catalog.listterm.CollectionConverter;
import org.jpc.converter.catalog.listterm.EnumerationConverter;
import org.jpc.converter.catalog.listterm.IterableConverter;
import org.jpc.converter.catalog.listterm.IteratorConverter;
import org.jpc.converter.catalog.listterm.MapConverter.MapToTermConverter;
import org.jpc.converter.catalog.listterm.MapConverter.TermToMapConverter;
import org.jpc.converter.catalog.listterm.MapEntryConverter.MapEntryToTermConverter;
import org.jpc.converter.catalog.listterm.MapEntryConverter.TermToMapEntryConverter;
import org.jpc.converter.typesolver.MapTypeSolver;
import org.jpc.term.Term;

public class DefaultJpcConverterManager extends ConverterManager<Object, Term> {

	public DefaultJpcConverterManager() {
		register(new IsoPrologErrorConverterManager());
		registerDefaultObjectConverters();
	}
	
	
	private void registerDefaultObjectConverters() {
		register(new ArrayConverter());
		register(new CollectionConverter());
		register(new EnumerationConverter());
		register(new IterableConverter());
		register(new IteratorConverter());
		register(new MapToTermConverter(MapTypeSolver.DEFAULT_MAP_ENTRY_SEPARATOR));
		register(new MapEntryToTermConverter(MapTypeSolver.DEFAULT_MAP_ENTRY_SEPARATOR));
		for(String mapEntrySeparator : MapTypeSolver.ALL_MAP_ENTRY_SEPARATORS) {
			register(new TermToMapConverter(mapEntrySeparator));
			register(new TermToMapEntryConverter(mapEntrySeparator));
		}
		register(new XmlGregorianCalendarConverter());
		register(new CalendarConverter());
		register(new CharacterConverter());
		register(new StringConverter());
		register(new BooleanConverter());
		register(new NumberConverter());
		register(new JRefConverter());
		register(new SerializedConverter());
	}
	

}
