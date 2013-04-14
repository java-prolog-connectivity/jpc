package org.jpc.converter.catalog.listterm;

import java.lang.reflect.Type;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.jpc.Jpc;
import org.jpc.converter.JpcConversionException;
import org.jpc.converter.JpcConverter;
import org.jpc.converter.catalog.listterm.MapEntryConverter.MapEntryToTermConverter;
import org.jpc.converter.catalog.listterm.MapEntryConverter.TermToMapEntryConverter;
import org.jpc.converter.typesolver.MapTypeSolver;
import org.jpc.term.Compound;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;
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
	
	@Override
	public <T extends Term> T toTerm(Map<K,V> map, Class<T> termClass, Jpc context) {
		if(!Term.class.isAssignableFrom(termClass))
			throw new JpcConversionException();
		ListTerm terms = new ListTerm();
		for(Entry<K,V> entry : map.entrySet()) {
			Term entryTerm = new MapEntryToTermConverter<K,V>(entrySeparator).toTerm(entry, Compound.class, context);
			terms.add(entryTerm);
		}
		return (T) terms.asTerm();
	}
	
	
	
	public static class MapToTermConverter<K,V> extends MapConverter<K,V> {
		
		public MapToTermConverter() {
			super();
		}
		
		public MapToTermConverter(String entrySeparator) {
			super(entrySeparator);
		}
		
		@Override
		public Map<K,V> fromTerm(Term term, Type type, Jpc context) {
			throw new UnsupportedOperationException();
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
		public <T extends Term> T toTerm(Map<K,V> map, Class<T> termClass, Jpc context) {
			throw new UnsupportedOperationException();
		}
		
	}
	
	
	
	
	


}
