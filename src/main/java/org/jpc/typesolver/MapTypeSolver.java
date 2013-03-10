package org.jpc.typesolver;

import java.lang.reflect.Type;
import java.util.Map;

import org.jpc.converter.catalog.listterm.MapConverter;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;

import com.google.common.base.Predicate;
import com.google.common.collect.Iterables;

public class MapTypeSolver implements TermTypeSolver {

	public static final String DEFAULT_MAP_ENTRY_SEPARATOR = "-";
	public static final String[] ALL_MAP_ENTRY_SEPARATORS = new String[]{DEFAULT_MAP_ENTRY_SEPARATOR, "="};
	
	@Override
	public Type getType(Term term) {
		if(term.isList()) {
			ListTerm list = term.asList();
			Predicate<Term> isMapEntry = new Predicate<Term>() {
				@Override
				public boolean apply(Term term) {
					if(term.arity() != 2)
						return false;
					for(String mapSeparator : ALL_MAP_ENTRY_SEPARATORS) {
						if(term.hasFunctor(mapSeparator, 2))
							return true;
					}
					return false;
				}
			};
			if(!list.isEmpty() && Iterables.all(list, isMapEntry))
				return Map.class;
		}
		return null;
	}

}
