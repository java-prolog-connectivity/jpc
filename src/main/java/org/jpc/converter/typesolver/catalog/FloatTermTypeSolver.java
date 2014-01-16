package org.jpc.converter.typesolver.catalog;

import java.lang.reflect.Type;

import org.jpc.converter.typesolver.TypeSolver;
import org.jpc.term.FloatTerm;

public class FloatTermTypeSolver implements TypeSolver<FloatTerm> {

	@Override
	public Type getType(FloatTerm floatTerm) {
		return Double.class;
	}

}
