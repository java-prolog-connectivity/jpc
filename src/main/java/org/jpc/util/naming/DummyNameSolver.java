package org.jpc.util.naming;

public class DummyNameSolver implements NameSolver {

	@Override
	public String nameOf(Nameable o) {
		String name = o.getName();
		if(name == null)
			name = "<NO NAME>";
		return name;
	}

}
