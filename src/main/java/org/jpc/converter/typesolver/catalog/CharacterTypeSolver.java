package org.jpc.converter.typesolver.catalog;

import java.lang.reflect.Type;

import org.jpc.converter.typesolver.TypeSolver;
import org.jpc.term.Atom;

public class CharacterTypeSolver implements TypeSolver<Character> {

	@Override
	public Type getType(Character object) {
		return Atom.class;
	}

}
