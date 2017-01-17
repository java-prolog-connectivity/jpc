package org.jpc.converter.typesolver.catalog;

import java.lang.reflect.Type;

import org.jpc.converter.typesolver.TypeSolver;
import org.jpc.term.Integer;

public class IntegerTermTypeSolver implements TypeSolver<Integer> {

	@Override
	public Type inferType(Integer integerTerm) {
		return Long.class;
	}

}
