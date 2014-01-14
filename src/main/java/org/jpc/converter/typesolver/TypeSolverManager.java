package org.jpc.converter.typesolver;

import java.lang.reflect.Type;

import org.jpc.converter.typesolver.catalog.CharacterTypeSolver;
import org.jpc.converter.typesolver.catalog.ConstantTermTypeSolver;
import org.jpc.converter.typesolver.catalog.ListTypeSolver;
import org.jpc.converter.typesolver.catalog.MapTypeSolver;
import org.jpc.converter.typesolver.catalog.NumberTypeSolver;
import org.jpc.converter.typesolver.catalog.StringTypeSolver;


public abstract class TypeSolverManager {
	
	public static final Object DEFAULT_KEY = new Object();
	
	/**
	 * Registers default type solvers in the given type solver manager.
	 * @param typeSolverManager a type solver manager.
	 */
	public static void registerDefaults(TypeSolverManager typeSolverManager) {
		typeSolverManager.register(new ConstantTermTypeSolver());
		typeSolverManager.register(new ListTypeSolver());
		typeSolverManager.register(new MapTypeSolver());
		typeSolverManager.register(new StringTypeSolver());
		typeSolverManager.register(new NumberTypeSolver());
		typeSolverManager.register(new CharacterTypeSolver());
	}
	
	public void register(TypeSolver<?> typeSolver) {
		register(DEFAULT_KEY, typeSolver);
	}
	
	public abstract void register(Object typeSolverKey, TypeSolver<?>  typeSolver);
	
	public Type getType(Object object) {
		return getType(DEFAULT_KEY, object);
	}
	
	public abstract Type getType(Object typeSolverKey, Object object);

}
