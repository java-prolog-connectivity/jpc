package org.jpc.mapping.converter.catalog.map;

import static java.util.Arrays.asList;
import static org.jconverter.converter.ConversionGoal.conversionGoal;

import java.lang.reflect.Type;
import java.util.AbstractMap;
import java.util.Map;
import java.util.Map.Entry;

import org.jconverter.converter.DelegateConversionException;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.mapping.converter.FromTermConverter;
import org.jpc.mapping.converter.ToTermConverter;
import org.jconverter.typesolver.catalog.MapTypeSolver;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.typeutils.typewrapper.TypeWrapper;


public abstract class MapEntryConverter<K,V> {

	protected String entrySeparator;

	public MapEntryConverter() {
		this(MapTypeSolver.DEFAULT_MAP_ENTRY_SEPARATOR);
	}
	
	public MapEntryConverter(String entrySeparator) {
		this.entrySeparator = entrySeparator;
	}
	

	public static class MapEntryToTermConverter<K,V> extends MapEntryConverter<K,V> implements ToTermConverter<Map.Entry<K,V>, Compound> {
		
		public MapEntryToTermConverter() {
			super();
		}
		
		public MapEntryToTermConverter(String entrySeparator) {
			super(entrySeparator);
		}
		
		@Override
		public Compound toTerm(Entry<K,V> entry, TypeDomain target, Jpc context) {
			Term key = context.toTerm(entry.getKey());
			Term value = context.toTerm(entry.getValue());
			Compound term = new Compound(entrySeparator, asList(key, value));
			return term;
		}

	}
	
	
	public static class TermToMapEntryConverter<K,V> extends MapEntryConverter<K,V> implements FromTermConverter<Compound, Map.Entry<K,V>> {
		
		public TermToMapEntryConverter() {
			super();
		}
		
		public TermToMapEntryConverter(String entrySeparator) {
			super(entrySeparator);
		}

		@Override
		public Entry<K,V> fromTerm(Compound term, TypeDomain target, Jpc context) {
			if (!term.hasFunctor(entrySeparator, 2)) {//verify the term structure
				throw new DelegateConversionException(conversionGoal(term, target));
			}
			if(!(target.getRawClass().equals(Entry.class) || target.getRawClass().equals(AbstractMap.SimpleEntry.class))) //verify the type
				throw new DelegateConversionException(conversionGoal(term, target));
			
			Term keyTerm = term.arg(1);
			Term valueTerm = term.arg(2);
			Type[] entryTypes = TypeWrapper.wrap(target.getType()).as(Entry.class).getActualTypeArgumentsOrUpperBounds();
			Type keyType = entryTypes[0];
			Type valueType = entryTypes[1];
			Object key = context.fromTerm(keyTerm, keyType);
			Object value = context.fromTerm(valueTerm, valueType);
			return new AbstractMap.SimpleEntry(key, value);
		}
		
	}

}