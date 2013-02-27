package org.jpc.converter.fromterm.fromlistterm;

import java.lang.reflect.Type;
import java.util.AbstractMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.fromterm.FromTermConverter;
import org.jpc.term.Term;
import org.jpc.typesolver.MapTypeSolver;
import org.minitoolbox.reflection.javatype.ParameterizedTypeImpl;
import org.minitoolbox.reflection.wrappertype.TypeWrapper;

public class ListTermToMapConverter<K,V> extends FromTermConverter<Map<K,V>> {
	
	private String entrySeparator;
	
	public ListTermToMapConverter() {
		this(MapTypeSolver.DEFAULT_MAP_ENTRY_SEPARATOR);
	}
	
	public ListTermToMapConverter(String entrySeparator) {
		this.entrySeparator = entrySeparator;
	}
	
	@Override
	public Map<K,V> convert(Term term, Type type, Jpc context) {
		Map<K,V> map = null;
		List<Term> listMembers = null;
		try {
			map = context.instantiate(type); //will throw an exception if the type is not compatible with map
			listMembers = term.asList(); //will throw an exception if the term is not a list term
		}catch(Exception e) {
			throw new JpcConversionException();
		}
		Type[] mapTypes = TypeWrapper.wrap(type).as(Map.class).getActualTypeArgumentsOrUpperBounds();
		Type entryType = new ParameterizedTypeImpl(mapTypes, Map.class, Map.Entry.class);
		
		for(Term termMember : listMembers) {
			Entry<K,V> entry = new TermToMapEntryConverter<K,V>(entrySeparator).convert(termMember, entryType, context);
			map.put(entry.getKey(), entry.getValue());
		}
		return map;
	}
	
	public static class TermToMapEntryConverter<K,V> extends FromTermConverter<Map.Entry<K,V>> {
		
		private String entrySeparator;

		public TermToMapEntryConverter() {
			this(MapTypeSolver.DEFAULT_MAP_ENTRY_SEPARATOR);
		}
		
		public TermToMapEntryConverter(String entrySeparator) {
			this.entrySeparator = entrySeparator;
		}
		
		@Override
		public Entry<K,V> convert(Term term, Type type, Jpc context) {
			if(!term.hasFunctor(entrySeparator, 2)) //verify the term structure
				throw new JpcConversionException();
			TypeWrapper typeWrapper = TypeWrapper.wrap(type);
			if(!(typeWrapper.getRawClass().equals(Entry.class) || typeWrapper.getRawClass().equals(AbstractMap.SimpleEntry.class))) //verify the type
				throw new JpcConversionException();
			
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
