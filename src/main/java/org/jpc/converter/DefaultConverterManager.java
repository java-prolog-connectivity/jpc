package org.jpc.converter;

import org.jpc.converter.fromterm.TermToBooleanConverter;
import org.jpc.converter.fromterm.TermToCalendarConverter;
import org.jpc.converter.fromterm.TermToCharConverter;
import org.jpc.converter.fromterm.TermToNumberConverter;
import org.jpc.converter.fromterm.TermToStringConverter;
import org.jpc.converter.fromterm.TermToXmlGregorianCalendarConverter;
import org.jpc.converter.fromterm.VariableTermToObjectConverter;
import org.jpc.converter.fromterm.fromlistterm.ListTermToArrayConverter;
import org.jpc.converter.fromterm.fromlistterm.ListTermToCollectionConverter;
import org.jpc.converter.fromterm.fromlistterm.ListTermToEnumerationConverter;
import org.jpc.converter.fromterm.fromlistterm.ListTermToMapConverter;
import org.jpc.converter.fromterm.fromlistterm.ListTermToMapConverter.TermToMapEntryConverter;
import org.jpc.converter.toterm.BooleanToTermConverter;
import org.jpc.converter.toterm.CalendarToTermConverter;
import org.jpc.converter.toterm.NumberToTermConverter;
import org.jpc.converter.toterm.StringToTermConverter;
import org.jpc.converter.toterm.XmlGregorianCalendarToTermConverter;
import org.jpc.converter.toterm.tolistterm.ArrayToTermConverter;
import org.jpc.converter.toterm.tolistterm.EnumerationToTermConverter;
import org.jpc.converter.toterm.tolistterm.IterableToTermConverter;
import org.jpc.converter.toterm.tolistterm.IteratorToTermConverter;
import org.jpc.converter.toterm.tolistterm.MapToTermConverter;
import org.jpc.converter.toterm.tolistterm.MapToTermConverter.MapEntryToTermConverter;
import org.jpc.typesolver.MapTypeSolver;

public class DefaultConverterManager extends ConverterManager {

	public DefaultConverterManager() {
		registerDefaultConverters();
	}
	
	private void registerDefaultConverters() {
		register(new ArrayToTermConverter());
		register(new EnumerationToTermConverter());
		register(new IterableToTermConverter());
		register(new IteratorToTermConverter());
		register(new MapToTermConverter(MapTypeSolver.DEFAULT_MAP_ENTRY_SEPARATOR));
		register(new MapEntryToTermConverter(MapTypeSolver.DEFAULT_MAP_ENTRY_SEPARATOR));
		register(new BooleanToTermConverter());
		register(new CalendarToTermConverter());
		register(new NumberToTermConverter());
		register(new StringToTermConverter());
		register(new XmlGregorianCalendarToTermConverter());
		
		register(new ListTermToArrayConverter());
		register(new ListTermToCollectionConverter());
		register(new ListTermToEnumerationConverter());
		for(String mapEntrySeparator : MapTypeSolver.MAP_ENTRY_SEPARATORS) {
			register(new ListTermToMapConverter(mapEntrySeparator)); 
			register(new TermToMapEntryConverter(mapEntrySeparator));
		}
		register(new ListTermToMapConverter("=")); 
		register(new TermToMapEntryConverter("="));
		register(new TermToBooleanConverter());
		register(new TermToCalendarConverter());
		register(new TermToNumberConverter());
		register(new TermToCharConverter());
		register(new TermToStringConverter());
		register(new TermToXmlGregorianCalendarConverter());
		register(new VariableTermToObjectConverter());
	}
	
}
