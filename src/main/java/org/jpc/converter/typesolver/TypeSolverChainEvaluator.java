package org.jpc.converter.typesolver;

import java.lang.reflect.Type;

import org.jconverter.converter.NonRedundantEvaluator;

import com.google.common.base.Function;

public class TypeSolverChainEvaluator<T> implements Function<Object, Type> {

	private final Function<TypeSolver<T>, Type> typeSolverEvaluator;
	
	public TypeSolverChainEvaluator(Function<TypeSolver<T>, Type> typeSolverEvaluator) {
		this.typeSolverEvaluator = typeSolverEvaluator;
	}
	
	@Override
	public Type apply(Object processingObject) {
		if(processingObject instanceof TypeSolver)
			return typeSolverEvaluator.apply((TypeSolver<T>) processingObject);
		else if(processingObject instanceof TypeSolverChain)
			return applyChain((TypeSolverChain)processingObject);
		else
			throw new RuntimeException("Wrong processing object.");
	}

	
	public Type applyChain(TypeSolverChain<T> typeSolverChain) {
		return typeSolverChain.apply((Function)this);
	}
	
	static class NonRedundantTypeSolverEvaluator<T> extends NonRedundantEvaluator<TypeSolver<T>,Type> {

		public NonRedundantTypeSolverEvaluator(Function<TypeSolver<T>,Type> evaluator) {
			super(evaluator);
		}
		
		@Override
		protected Type onAlreadyVisited(TypeSolver<T> alreadyVisited) {
			throw new UnrecognizedObjectException();
		}
		
	}
	
}
