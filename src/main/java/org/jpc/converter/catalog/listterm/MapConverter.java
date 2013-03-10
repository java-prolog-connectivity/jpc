package org.jpc.converter.catalog.listterm;

import static java.util.Arrays.asList;

import java.lang.reflect.Type;
import java.util.AbstractMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.JpcConverter;
import org.jpc.converter.catalog.listterm.MapConverter.MapEntryConverter.MapEntryToTermConverter;
import org.jpc.converter.catalog.listterm.MapConverter.MapEntryConverter.TermToMapEntryConverter;
import org.jpc.term.Compound;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;
import org.jpc.typesolver.MapTypeSolver;
import org.minitoolbox.reflection.javatype.ParameterizedTypeImpl;
import org.minitoolbox.reflection.wrappertype.TypeWrapper;

public abstract class MapConverter<K,V> extends JpcConverter<Map<K,V>, Term> {

	protected String entrySeparator;
	
	public MapConverter() {
		this(MapTypeSolver.DEFAULT_MAP_ENTRY_SEPARATOR);
	}

	public MapConverter(String entrySeparator) {
		this.entrySeparator = entrySeparator;
	}
	

	public static class MapToTermConverter<K,V> extends MapConverter<K,V> {
		
		public MapToTermConverter() {
			super();
		}
		
		public MapToTermConverter(String entrySeparator) {
			super(entrySeparator);
		}
		
		@Override
		public <T extends Term> T toTerm(Map<K,V> map, Class<T> termClass, Jpc context) {
			if(!Term.class.isAssignableFrom(termClass))
				throw new JpcConversionException();
			ListTerm terms = new ListTerm();
			for(Entry<K,V> entry : map.entrySet()) {
				Term entryTerm = new MapEntryToTermConverter<K,V>(entrySeparator).toTerm(entry, context);
				terms.add(entryTerm);
			}
			return (T) terms.asTerm();
		}
	}
	
	public static class TermToMapConverter<K,V> extends MapConverter<K,V> {
		
		public TermToMapConverter() {
			super();
		}
		
		public TermToMapConverter(String entrySeparator) {
			super(entrySeparator);
		}
		
		@Override
		public Map<K,V> fromTerm(Term term, Type type, Jpc context) {
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
				Entry<K,V> entry = new TermToMapEntryConverter<K,V>(entrySeparator).fromTerm((Compound)termMember, entryType, context);
				map.put(entry.getKey(), entry.getValue());
			}
			return map;
		}
		
	}
	
	
	
	
	
	public static abstract class MapEntryConverter<K,V> extends JpcConverter<Entry<K,V>, Compound> {

		protected String entrySeparator;

		public MapEntryConverter() {
			this(MapTypeSolver.DEFAULT_MAP_ENTRY_SEPARATOR);
		}
		
		public MapEntryConverter(String entrySeparator) {
			this.entrySeparator = entrySeparator;
		}
		
		
		public static class MapEntryToTermConverter<K,V> extends MapEntryConverter<K,V> {
			
			public MapEntryToTermConverter() {
				super();
			}
			
			public MapEntryToTermConverter(String entrySeparator) {
				super(entrySeparator);
			}
			
			@Override
			public <T extends Compound> T toTerm(Entry<K,V> entry, Class<T> termClass, Jpc context) {
				Term key = context.toTerm(entry.getKey());
				Term value = context.toTerm(entry.getValue());
				Compound term = new Compound(entrySeparator, asList(key, value));
				return (T) term;
			}
			
		}
		
		
		public static class TermToMapEntryConverter<K,V> extends MapEntryConverter<K,V> {
			
			public TermToMapEntryConverter() {
				super();
			}
			
			public TermToMapEntryConverter(String entrySeparator) {
				super(entrySeparator);
			}
			
			@Override
			public Entry<K,V> fromTerm(Compound term, Type type, Jpc context) {
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

}
