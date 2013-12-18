package org.jpc.converter.catalog.map;

import static java.util.Arrays.asList;

import java.lang.reflect.Type;
import java.util.AbstractMap;
import java.util.Map;
import java.util.Map.Entry;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.converter.typesolver.catalog.MapTypeSolver;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;

public abstract class MapEntryConverter<K,V> {

	protected String entrySeparator;

	public MapEntryConverter() {
		this(MapTypeSolver.DEFAULT_MAP_ENTRY_SEPARATOR);
	}
	
	public MapEntryConverter(String entrySeparator) {
		this.entrySeparator = entrySeparator;
	}
	

	public static class MapEntryToTermConverter extends MapEntryConverter implements ToTermConverter<Map.Entry, Compound> {
		
		public MapEntryToTermConverter() {
			super();
		}
		
		public MapEntryToTermConverter(String entrySeparator) {
			super(entrySeparator);
		}
		
		@Override
		public Compound toTerm(Entry entry, Class<Compound> termClass, Jpc context) {
			Term key = context.toTerm(entry.getKey());
			Term value = context.toTerm(entry.getValue());
			Compound term = new Compound(entrySeparator, asList(key, value));
			return term;
		}

	}
	
	
	public static class TermToMapEntryConverter<K,V> extends MapEntryConverter implements FromTermConverter<Compound, Map.Entry> {
		
		public TermToMapEntryConverter() {
			super();
		}
		
		public TermToMapEntryConverter(String entrySeparator) {
			super(entrySeparator);
		}

		@Override
		public Entry<K,V> fromTerm(Compound term, Type type, Jpc context) {
			if(!term.hasFunctor(entrySeparator, 2)) //verify the term structure
				throw new ConversionException();
			TypeWrapper typeWrapper = TypeWrapper.wrap(type);
			if(!(typeWrapper.getRawClass().equals(Entry.class) || typeWrapper.getRawClass().equals(AbstractMap.SimpleEntry.class))) //verify the type
				throw new ConversionException();
			
			Term keyTerm = term.arg(1);
			Term valueTerm = term.arg(2);
			Type[] entryTypes = TypeWrapper.wrap(type).as(Entry.class).getActualTypeArgumentsOrUpperBounds();
			Type keyType = entryTypes[0];
			Type valueType = entryTypes[1];
			Object key = context.fromTerm(keyTerm, keyType);
			Object value = context.fromTerm(valueTerm, valueType);
			return new AbstractMap.SimpleEntry(key, value);
		}
		
	}

}