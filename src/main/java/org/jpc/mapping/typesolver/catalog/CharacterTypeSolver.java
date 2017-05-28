package org.jpc.mapping.typesolver.catalog;

import java.lang.reflect.Type;

import org.jconverter.typesolver.TypeSolver;
import org.jpc.term.Atom;

public class CharacterTypeSolver implements TypeSolver<Character> {

	@Override
	public Type inferType(Character object) {
		return Atom.class;
	}

}
