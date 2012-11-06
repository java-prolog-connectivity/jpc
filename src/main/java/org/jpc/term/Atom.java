package org.jpc.term;

import static com.google.common.base.Preconditions.checkNotNull;

import java.util.Collections;
import java.util.List;

import org.jpc.engine.visitor.AbstractJpcVisitor;

/**
 * A class reifying a logic atom
 * @author scastro
 *
 */
public class Atom extends Term {

	public static final Term TRUE_TERM = new Atom("true");
	public static final Term FALSE_TERM = new Atom("false");
	
	protected final String name;
	
	/**
	 * @param   name   the Atom's name (unquoted)
	 */
	public Atom(String name) {
		checkNotNull(name);
		this.name = name;
	}
	
	public String name() {
		return name;
	}


	@Override
	public boolean hasFunctor(TermAdaptable nameTermObject, int arity) {
		return termEquivalent(nameTermObject) && arity == 0;
	}
	
	public boolean equals(Object obj) {
		return (this == obj || (obj instanceof Atom && name.equals(((Atom)obj).name)));
	}
	
	@Override
	public String toString() {
		return prologName();
	}
	
	/**
	 *   This method should return the Prolog representation of an atom (the name of the compound)
	 * 	 for example, 'name' is quoted if necessary or escaping characters are added before special characters
	 *   by default this class just surrounds the name with single quotes (but this could be overridden if necessary).
	 *   This method affects only the string representation of the term
	 */
	private String prologName() {
		return "'" + name + "'";
	}

	@Override
	public void accept(AbstractJpcVisitor termVisitor) {
		termVisitor.visitAtom(this);
	}

}
