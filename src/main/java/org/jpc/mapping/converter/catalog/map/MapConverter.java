package org.jpc.mapping.converter.catalog.map;

import static org.jconverter.converter.ConversionGoal.conversionGoal;
import static org.jconverter.converter.TypeDomain.typeDomain;
import static org.jpc.internal.reflection.ReflectionUtil.parameterizedType;

import java.lang.reflect.Type;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.jconverter.converter.DelegateConversionException;
import org.jconverter.converter.TypeDomain;
import org.jpc.Jpc;
import org.jpc.mapping.converter.FromTermConverter;
import org.jpc.mapping.converter.ToTermConverter;
import org.jpc.mapping.converter.catalog.map.MapEntryConverter.MapEntryToTermConverter;
import org.jpc.mapping.converter.catalog.map.MapEntryConverter.TermToMapEntryConverter;
import org.jconverter.typesolver.catalog.MapTypeSolver;
import org.jpc.term.Compound;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;
import org.typeutils.typewrapper.TypeWrapper;


public abstract class MapConverter {

	protected String entrySeparator;
	
	public MapConverter() {
		this(MapTypeSolver.DEFAULT_MAP_ENTRY_SEPARATOR);
	}

	public MapConverter(String entrySeparator) {
		this.entrySeparator = entrySeparator;
	}
	

	public static class MapToTermConverter<T extends Map<?,?>,U extends Term> extends MapConverter implements ToTermConverter<T,U> {

		public MapToTermConverter(String entrySeparator) {
			super(entrySeparator);
		}
		
		@Override
		public U toTerm(T map, TypeDomain target, Jpc context) {
			if (!target.isSubsetOf(typeDomain(Term.class))) {
				throw new DelegateConversionException(conversionGoal(map, target));
			}
			ListTerm terms = new ListTerm();
			Set<Entry<?,?>> entries = (Set)map.entrySet();
			for(Entry<?,?> entry : entries) {
				Term entryTerm = new MapEntryToTermConverter(entrySeparator).toTerm(entry, Compound.class, context);
				terms.add(entryTerm);
			}
			return (U) terms.asTerm();
		}
	}
	
	
	public static class TermToMapConverter<T extends Term,U extends Map<?,?>> extends MapConverter implements FromTermConverter<T,U> {

		public TermToMapConverter(String entrySeparator) {
			super(entrySeparator);
		}
		
		@Override
		public U fromTerm(T listTerm, TypeDomain target, Jpc context) {
			if (!listTerm.isList()) {
				throw new DelegateConversionException(conversionGoal(listTerm, target));
			}
			U map = null;
			List<Term> listMembers = null;
			try {
				map = context.instantiate(target.getType()); //will throw an exception if the type is not compatible with map
				listMembers = listTerm.asList(); //will throw an exception if the term is not a list term
			}catch(Exception e) {
				throw new DelegateConversionException(conversionGoal(listTerm, target));
			}
			Type[] mapTypes = TypeWrapper.wrap(target.getType()).as(Map.class).getActualTypeArgumentsOrUpperBounds();
			Type entryType = parameterizedType(mapTypes, Map.class, Map.Entry.class);
			
			for(Term termMember : listMembers) {
				Entry<?,?> entry = new TermToMapEntryConverter(entrySeparator).fromTerm((Compound)termMember, typeDomain(entryType), context);
				((Map)map).put(entry.getKey(), entry.getValue());
			}
			return map;
		}
		
	}

}
