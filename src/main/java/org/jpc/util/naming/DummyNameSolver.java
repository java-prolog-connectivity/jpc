package org.jpc.util.naming;

public class DummyNameSolver implements NameSolver {

	@Override
	public String nameOf(Nameable o) {
		return o.getName();
	}

}
