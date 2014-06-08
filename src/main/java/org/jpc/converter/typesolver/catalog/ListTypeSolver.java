package org.jpc.converter.typesolver.catalog;

import java.lang.reflect.Type;
import java.util.List;

import org.jpc.converter.typesolver.TypeSolver;
import org.jpc.converter.typesolver.UnrecognizedObjectException;
import org.jpc.term.Compound;

public class ListTypeSolver implements TypeSolver<Compound> {

	@Override
	public Type inferType(Compound term) {
		if(term.isList())
			return List.class;
		throw new UnrecognizedObjectException();
	}

}
