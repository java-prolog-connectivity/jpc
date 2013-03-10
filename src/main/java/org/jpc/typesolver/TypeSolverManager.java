package org.jpc.typesolver;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

import org.jpc.term.Term;

public class TypeSolverManager implements TermTypeSolver {

	List<TermTypeSolver> typeSolvers;
	
	public TypeSolverManager() {
		typeSolvers = new ArrayList<>();
	}
	
	public void register(TermTypeSolver typeSolver) {
		typeSolvers.add(0, typeSolver);
	}
	
	public Type getType(Term term) {
		for(TermTypeSolver typeSolver : typeSolvers) {
			Type type = typeSolver.getType(term);
			if(type != null)
				return type;
		}
		return null;
	}
	
}
