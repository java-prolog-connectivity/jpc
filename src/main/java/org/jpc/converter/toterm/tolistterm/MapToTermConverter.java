package org.jpc.converter.toterm.tolistterm;

import static java.util.Arrays.asList;

import java.util.Map;
import java.util.Map.Entry;

import org.jpc.Jpc;
import org.jpc.converter.toterm.ToTermConverter;
import org.jpc.term.Compound;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;
import org.jpc.typesolver.MapTypeSolver;

public class MapToTermConverter extends ToTermConverter<Map<?,?>> {

	private String entrySeparator;
	
	public MapToTermConverter() {
		this(MapTypeSolver.DEFAULT_MAP_ENTRY_SEPARATOR);
	}
	
	public MapToTermConverter(String entrySeparator) {
		this.entrySeparator = entrySeparator;
	}
	
	@Override
	public Term convert(Map<?,?> map, Jpc context) {
		ListTerm terms = new ListTerm();
		for(Entry<?,?> entry : map.entrySet()) {
			Term entryTerm = new MapEntryToTermConverter(entrySeparator).convert(entry, context);
			terms.add(entryTerm);
		}
		return terms.asTerm();
	}

	public static class MapEntryToTermConverter extends ToTermConverter<Entry<?,?>> {

		private String entrySeparator;

		public MapEntryToTermConverter() {
			this(MapTypeSolver.DEFAULT_MAP_ENTRY_SEPARATOR);
		}
		
		public MapEntryToTermConverter(String entrySeparator) {
			this.entrySeparator = entrySeparator;
		}
		
		@Override
		public Term convert(Entry<?,?> entry, Jpc context) {
			Term key = context.toTerm(entry.getKey());
			Term value = context.toTerm(entry.getValue());
			Term term = new Compound(entrySeparator, asList(key, value));
			return term;
		}
	}

}
