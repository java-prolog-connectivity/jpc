package org.jpc.converter.typesolver;

import java.lang.reflect.Type;

import com.google.common.base.Function;

public class TypeSolverEvaluator<T> implements Function<TypeSolver<T>, Type> {

	private final T object;

	public TypeSolverEvaluator(T object) {
		this.object = object;
	}

	@Override
	public Type apply(TypeSolver<T> typeSolver) {
		return typeSolver.getType(object);
	}
	
}
