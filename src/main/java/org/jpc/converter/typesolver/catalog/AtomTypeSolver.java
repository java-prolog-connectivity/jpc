package org.jpc.converter.typesolver.catalog;

import java.lang.reflect.Type;
import java.util.List;

import org.jpc.converter.typesolver.TypeSolver;
import org.jpc.term.Atom;

public class AtomTypeSolver implements TypeSolver<Atom> {

	@Override
	public Type getType(Atom atom) {
		if(atom.isList())
			return List.class;
		else if(atom.isBoolean())
			return Boolean.class;
		else
			return String.class;
	}

}
