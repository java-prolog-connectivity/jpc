package org.jpc.converter.typesolver;

public class DefaultTypeSolverManager extends TypeSolverManager {

	public DefaultTypeSolverManager() {
		register(new PrimitiveTypeSolver());
		register(new ListTypeSolver());
		register(new MapTypeSolver());
	}
}