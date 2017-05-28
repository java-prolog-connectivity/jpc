package org.jpc.mapping.typesolver.catalog;

import java.lang.reflect.Type;
import java.util.List;

import org.jconverter.typesolver.UnrecognizedObjectException;
import org.jconverter.typesolver.TypeSolver;
import org.jpc.term.Compound;

public class ListTypeSolver implements TypeSolver<Compound> {

	@Override
	public Type inferType(Compound term) {
		if(term.isList())
			return List.class;
		throw new UnrecognizedObjectException();
	}

}
