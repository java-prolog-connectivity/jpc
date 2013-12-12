package org.jpc.converter.typesolver;

import java.lang.reflect.Type;
import java.util.List;

import org.jgum.strategy.ChainOfResponsibility;

public class TypeSolverChain<T> extends ChainOfResponsibility<TypeSolver<T>, Type> /*implements TypeSolver<T>*/ {

	public TypeSolverChain() {
		super(UnrecognizedObjectException.class);
	}
	
	public TypeSolverChain(List<TypeSolver<T>> responsibilityChain) {
		super(responsibilityChain, UnrecognizedObjectException.class);
	}
	
//	@Override
//	public Type getType(T object) {
//		return apply(new TypeSolverChainEvaluator<T>(object));
//	}

}
