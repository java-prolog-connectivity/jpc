package org.jpc.converter.typesolver.catalog;

import java.lang.reflect.Type;

import org.jpc.converter.typesolver.TypeSolver;
import org.jpc.term.Atom;

public class StringTypeSolver implements TypeSolver<String> {

	@Override
	public Type getType(String object) {
		return Atom.class;
	}

}
