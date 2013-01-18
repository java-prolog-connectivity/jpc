package org.jpc.converter.toterm;

import static java.util.Arrays.asList;

import java.util.Map.Entry;

import org.jpc.term.Compound;
import org.jpc.term.Term;

public class MapEntryToTermConverter implements ToTermConverter<Entry> {

	public static final String DEFAULT_ENTRY_SEPARATOR = "-";
	
	private ToTermConverter<Object> keyConverter;
	private ToTermConverter<Object> valueConverter;
	public String entrySeparator;
	
	public MapEntryToTermConverter() {
		this(new DefaultToTermConverter(), new DefaultToTermConverter());
	}
	
	public MapEntryToTermConverter(ToTermConverter keyConverter, ToTermConverter valueConverter) {
		this(keyConverter, valueConverter, DEFAULT_ENTRY_SEPARATOR);
	}
	
	public MapEntryToTermConverter(ToTermConverter keyConverter, ToTermConverter valueConverter, String entrySeparator) {
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
