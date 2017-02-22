package org.jpc.converter.typesolver;

import java.lang.reflect.Type;
import java.util.function.Function;

import org.jconverter.util.NonRedundantEvaluator;

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


	static class NonRedundantTypeSolverEvaluator<T> extends TypeSolverEvaluator<T> {

		private final NonRedundantEvaluator<TypeSolver<T>, Type> nonRedundantEvaluator;

		public NonRedundantTypeSolverEvaluator(TypeSolverEvaluator<T> evaluator) {
			super(evaluator.getSourceObject());
			this.nonRedundantEvaluator = new NonRedundantEvaluator<>(evaluator);
		}

		@Override
		public Type apply(TypeSolver<T> typeSolver) {
			try {
				return nonRedundantEvaluator.apply(typeSolver);
			} catch(NonRedundantEvaluator.AlreadyEvaluatedException e) {
				throw new UnrecognizedObjectException();
			}
		}
	}
	
}
