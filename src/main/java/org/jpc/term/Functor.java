package org.jpc.term;

import java.util.ArrayList;
import java.util.List;


public class Functor {

	private final Term name;
	private final int arity;
	
	public Functor(String name, int arity) {
		this(new Atom(name), arity);
	}
	
	public Functor(Term name, int arity) {
		this.name = name;
		this.arity = arity;
	}

	public Term getName() {
		return name;
	}

	public int getArity() {
		return arity;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + arity;
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Functor other = (Functor) obj;
		if (arity != other.arity)
			return false;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return name.toString() + "/" + arity;
	}

	public Term asTerm() {
		if(arity == 0)
			return name;
		else {
			List<Term> args = new ArrayList<>();
			for(int i = 0 ; i<arity; i++) {
				args.add(Var.ANONYMOUS_VAR);
			}
			return new Compound(name, args);
		}
	}

}
