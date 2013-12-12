package org.jpc.converter.catalog.map;

import java.lang.reflect.Type;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.jconverter.converter.ConversionException;
import org.jpc.Jpc;
import org.jpc.converter.FromTermConverter;
import org.jpc.converter.ToTermConverter;
import org.jpc.converter.catalog.map.MapEntryConverter.MapEntryToTermConverter;
import org.jpc.converter.catalog.map.MapEntryConverter.TermToMapEntryConverter;
import org.jpc.converter.typesolver.catalog.MapTypeSolver;
import org.jpc.term.Compound;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;
import org.minitoolbox.reflection.javatype.ParameterizedTypeImpl;
import org.minitoolbox.reflection.typewrapper.TypeWrapper;

public abstract class MapConverter {

	protected String entrySeparator;
	
	public MapConverter() {
		this(MapTypeSolver.DEFAULT_MAP_ENTRY_SEPARATOR);
	}

	public MapConverter(String entrySeparator) {
		this.entrySeparator = entrySeparator;
	}
	
	
	
	
	
	
	
	public static class MapToTermConverter<T extends Map,U extends Term> extends MapConverter implements ToTermConverter<T,U> {
		
		public MapToTermConverter() {
			super();
		}
		
		public MapToTermConverter(String entrySeparator) {
			super(entrySeparator);
		}
		
		@Override
		public U toTerm(T map, Class<U> termClass, Jpc context) {
			if(!Term.class.isAssignableFrom(termClass))
				throw new ConversionException();
			ListTerm terms = new ListTerm();
			Set<Entry> entries = map.entrySet();
			for(Entry entry : entries) {
				Term entryTerm = new MapEntryToTermConverter(entrySeparator).toTerm(entry, Compound.class, context);
				terms.add(entryTerm);
			}
			return (U) terms.asTerm();
		}
		
	}
	
	
	public static class TermToMapConverter<T extends Term,U extends Map> extends MapConverter implements FromTermConverter<T,U> {
		
		public TermToMapConverter() {
			super();
		}
		
		public TermToMapConverter(String entrySeparator) {
			super(entrySeparator);
		}
		
		@Override
		public U fromTerm(T term, Type type, Jpc context) {
			U map = null;
			List<Term> listMembers = null;
			try {
				map = context.instantiate(type); //will throw an exception if the type is not compatible with map
				listMembers = term.asList(); //will throw an exception if the term is not a list term
			}catch(Exception e) {
				throw new ConversionException();
			}
			Type[] mapTypes = TypeWrapper.wrap(type).as(Map.class).getActualTypeArgumentsOrUpperBounds();
			Type entryType = new ParameterizedTypeImpl(mapTypes, Map.class, Map.Entry.class);
			
			for(Term termMember : listMembers) {
				Entry entry = new TermToMapEntryConverter(entrySeparator).fromTerm((Compound)termMember, entryType, context);
				map.put(entry.getKey(), entry.getValue());
			}
			return map;
		}
		
	}

}
