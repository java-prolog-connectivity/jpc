package org.jpc.converter.typesolver;

import java.lang.reflect.Type;

import java.util.function.Function;

public class TypeSolverEvaluator<T> implements Function<TypeSolver<T>, Type> {

	private final T sourceObject;

	public TypeSolverEvaluator(T sourceObject) {
		this.sourceObject = sourceObject;
	}

	public T getSourceObject() {
		return sourceObject;
	}

	@Override
	public Type apply(TypeSolver<T> typeSolver) {
		return typeSolver.inferType(sourceObject);
	}
	
}
