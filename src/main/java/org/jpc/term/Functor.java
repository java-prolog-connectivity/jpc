package org.jpc.term;

import static org.jpc.term.Atom.atom;
import static org.jpc.term.Var.dontCare;

import java.util.ArrayList;
import java.util.List;

import org.jpc.mapping.converter.TermConvertable;


public class Functor implements TermConvertable<Term> {

	private final Term name;
	private final int arity;

	private Functor(Term name, int arity) {
		this.name = name;
		this.arity = arity;
	}

	public static Functor functor(String name, int arity) {
		return new Functor(atom(name), arity);
	}

	public static Functor functor(Term name, int arity) {
		return new Functor(name, arity);
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

	@Override
	public Term asTerm() {
		if(arity == 0)
			return name;
		else {
			List<Term> args = new ArrayList<>();
			for(int i = 0 ; i<arity; i++) {
				args.add(dontCare());
			}
			return new Compound(name, args);
		}
	}

}
