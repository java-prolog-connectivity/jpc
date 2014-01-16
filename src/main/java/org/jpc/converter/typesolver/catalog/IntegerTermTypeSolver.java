package org.jpc.converter.typesolver.catalog;

import java.lang.reflect.Type;

import org.jpc.converter.typesolver.TypeSolver;
import org.jpc.term.IntegerTerm;

public class IntegerTermTypeSolver implements TypeSolver<IntegerTerm> {

	@Override
	public Type getType(IntegerTerm integerTerm) {
		return Long.class;
	}

}
