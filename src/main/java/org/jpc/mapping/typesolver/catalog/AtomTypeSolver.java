package org.jpc.mapping.typesolver.catalog;

import java.lang.reflect.Type;
import java.util.List;

import org.jconverter.typesolver.TypeSolver;
import org.jpc.term.Atom;

public class AtomTypeSolver implements TypeSolver<Atom> {

	@Override
	public Type inferType(Atom atom) {
		if (atom.isList()) {
			return List.class;
		} else if(atom.isBoolean()) {
			return Boolean.class;
		} else {
			return String.class;
		}

	}

}
