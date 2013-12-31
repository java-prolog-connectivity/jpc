package org.jpc.term.compiled;

import org.jpc.term.Atom;
import org.jpc.term.Term;

public class InternedAtom extends Atom {
	
	public InternedAtom(String name) {
		super(name.intern());
	}
	
	@Override
	public Term compile(int clauseId, CompilationContext context) {
		return this;
	}

	@Override
	public Term compileForQuery(CompilationContext context) {
		return this;
	}

	@Override
	public Term forEnvironment(int environmentId) {
		return this;
	}
	
	@Override
	public int hashCode() {
		return System.identityHashCode(getName());
	}
	
	@Override
	public boolean equals(Object obj) {
		return (this == obj || (obj.getClass().equals(InternedAtom.class) && getName() == (((Atom)obj).getName())));
	}
	
}