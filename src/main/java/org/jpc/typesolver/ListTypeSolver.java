package org.jpc.typesolver;

import java.lang.reflect.Type;
import java.util.List;

import org.jpc.term.Term;

public class ListTypeSolver implements TermTypeSolver {

	@Override
	public Type getType(Term term) {
		if(term.isList())
			return List.class;
		else
			return null;
	}

}
