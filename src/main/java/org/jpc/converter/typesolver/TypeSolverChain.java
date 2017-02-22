package org.jpc.converter.typesolver;

import java.lang.reflect.Type;
import java.util.List;
import java.util.function.Function;

import org.jgum.ChainOfResponsibilityExhaustedException;
import org.jgum.strategy.ChainOfResponsibility;

public class TypeSolverChain<T> extends ChainOfResponsibility<TypeSolver<T>, Type> /*implements TypeSolver<T>*/ {

	public TypeSolverChain(List<TypeSolver<T>> responsibilityChain) {
		super(responsibilityChain, UnrecognizedObjectException.class);
	}

	@Override
	public Type apply(Function<TypeSolver<T>, Type> evaluator) {
		try {
			return super.apply(evaluator);
		} catch(ChainOfResponsibilityExhaustedException e) {
			throw new UnrecognizedObjectException(e);
		}
	}

//	@Override
//	public Type inferType(T object) {
//		return apply(new TypeSolverChainEvaluator<T>(object));
//	}

}
