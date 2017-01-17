package org.jpc.converter.typesolver.catalog;

import java.lang.reflect.Type;

import org.jpc.converter.typesolver.TypeSolver;
import org.jpc.term.Float;

public class FloatTermTypeSolver implements TypeSolver<Float> {

	@Override
	public Type inferType(Float floatTerm) {
		return Double.class;
	}

}
