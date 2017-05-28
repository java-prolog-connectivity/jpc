package org.jpc.mapping.typesolver.catalog;

import java.lang.reflect.Type;

import org.jconverter.typesolver.TypeSolver;
import org.jpc.term.Atom;

public class StringTypeSolver implements TypeSolver<String> {

	@Override
	public Type inferType(String object) {
		return Atom.class;
	}

}
