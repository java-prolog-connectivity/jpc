package org.jpc.converter.catalog.list;

import java.lang.reflect.Type;

import org.jpc.converter.ConverterManager;
import org.jpc.converter.catalog.map.MapConverter.MapToTermConverter;
import org.jpc.converter.catalog.map.MapConverter.TermToMapConverter;
import org.jpc.converter.typesolver.MapTypeSolver;
import org.jpc.term.Term;

public class ListTermConverterManager extends ConverterManager<Object, Term> {

	public ListTermConverterManager() {
		register(new ArrayConverter());
		register(new CollectionConverter());
		register(new EnumerationConverter());
		register(new IterableConverter());
		register(new IteratorConverter());
		register(new MapToTermConverter(MapTypeSolver.DEFAULT_MAP_ENTRY_SEPARATOR));
		for(String mapEntrySeparator : MapTypeSolver.ALL_MAP_ENTRY_SEPARATORS) {
			register(new TermToMapConverter(mapEntrySeparator));
		}
	}
	
	@Override
	public boolean canConvertFromTerm(Term term, Type toType) {
		return super.canConvertFromTerm(term, toType) && term.isList();
	}
}
