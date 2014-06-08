package org.jpc.converter.typesolver.catalog;

import java.lang.reflect.Type;
import java.util.Map;

import org.jpc.converter.typesolver.TypeSolver;
import org.jpc.converter.typesolver.UnrecognizedObjectException;
import org.jpc.term.Compound;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;

import com.google.common.base.Predicate;
import com.google.common.collect.Iterables;

public class MapTypeSolver implements TypeSolver<Compound> {

	public static final String DEFAULT_MAP_ENTRY_SEPARATOR = ":";
	public static final String[] ALL_MAP_ENTRY_SEPARATORS = new String[]{DEFAULT_MAP_ENTRY_SEPARATOR, "-", "="};
	
	@Override
	public Type inferType(Compound term) {
		if(term.isList()) {
			ListTerm list = term.asList();
			Predicate<Term> isMapEntry = new Predicate<Term>() {
				@Override
				public boolean apply(Term term) {
					return isMapEntry(term);
				}
			};
			if(!list.isEmpty() && Iterables.all(list, isMapEntry))
				return Map.class;
		}
		throw new UnrecognizedObjectException();
	}

	private boolean isMapEntry(Term term) {
		if(term.arity() != 2)
			return false;
		for(String mapSeparator : ALL_MAP_ENTRY_SEPARATORS) {
			if(term.hasFunctor(mapSeparator, 2))
				return true;
		}
		return false;
	}
}
