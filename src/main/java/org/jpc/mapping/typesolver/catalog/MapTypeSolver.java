package org.jpc.mapping.typesolver.catalog;

import java.lang.reflect.Type;
import java.util.Map;

import org.jconverter.typesolver.TypeSolver;
import org.jconverter.typesolver.UnrecognizedObjectException;
import org.jpc.term.Compound;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;

public class MapTypeSolver implements TypeSolver<Compound> {

	public static final String DEFAULT_MAP_ENTRY_SEPARATOR = ":";
	public static final String[] ALL_MAP_ENTRY_SEPARATORS = new String[]{DEFAULT_MAP_ENTRY_SEPARATOR, "-", "="};
	
	@Override
	public Type inferType(Compound term) {
		if(term.isList()) {
			ListTerm list = term.asList();
			if (!list.isEmpty() && list.stream().allMatch(this::isMapEntry)) {
				return Map.class;
			}

		}
		throw new UnrecognizedObjectException();
	}

	private boolean isMapEntry(Term term) {
		if (term.getArity() != 2) {
			return false;
		}

		for(String mapSeparator : ALL_MAP_ENTRY_SEPARATORS) {
			if(term.hasFunctor(mapSeparator, 2)) {
				return true;
			}
		}
		return false;
	}
}
