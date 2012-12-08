package org.jpc.engine;

import org.jpc.term.Atom;
import org.jpc.term.Term;
import org.jpc.term.TermConvertable;

public abstract class Flag implements TermConvertable {

	private String name;
	
	public Flag(String name) {
		this.name = name;
	}
	
	public String getName() {
		return name;
	}
	
	@Override
	public String toString() {return getName();}

	@Override
	public Term asTerm() {
		return new Atom(name);
	}

}
