package org.jpc.converter.toterm;

import static java.util.Arrays.asList;

import java.util.Map.Entry;

import org.jpc.term.Compound;
import org.jpc.term.Term;

public class MapEntryToTermConverter implements ObjectToTermConverter<Entry> {

	public static final String DEFAULT_ENTRY_SEPARATOR = "-";
	
	private ObjectToTermConverter<Object> keyConverter;
	private ObjectToTermConverter<Object> valueConverter;
	public String entrySeparator;
	
	public MapEntryToTermConverter(ObjectToTermConverter keyConverter, ObjectToTermConverter valueConverter) {
		this(keyConverter, valueConverter, DEFAULT_ENTRY_SEPARATOR);
	}
	
	public MapEntryToTermConverter(ObjectToTermConverter keyConverter, ObjectToTermConverter valueConverter, String entrySeparator) {
		this.keyConverter = keyConverter;
		this.valueConverter = valueConverter;
		this.entrySeparator = entrySeparator;
	}
	
	@Override
	public Term apply(Entry entry) {
		Term key = keyConverter.apply(entry.getKey());
		Term value = valueConverter.apply(entry.getValue());
		Term term = new Compound(entrySeparator, asList(key, value));
		return term;
	}

}
