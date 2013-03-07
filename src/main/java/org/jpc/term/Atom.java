package org.jpc.term;

import static org.jpc.engine.prolog.PrologConstants.EMPTY_LIST_SYMBOL;
import static org.jpc.engine.prolog.PrologConstants.FAIL;
import static org.jpc.engine.prolog.PrologConstants.FALSE;
import static org.jpc.engine.prolog.PrologConstants.TRUE;

import org.jpc.JpcException;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.salt.TermContentHandler;
import org.jpc.term.visitor.TermVisitor;

/**
 * A class reifying a logic atom
 * @author scastro
 *
 */
public final class Atom extends AbstractTerm {

	public static final Term TRUE_TERM = new Atom(TRUE);
	public static final Term FALSE_TERM = new Atom(FAIL); //choosing 'fail' over 'false', since 'fail' is ISO
	public static final Term EMPTY_LIST = new Atom(EMPTY_LIST_SYMBOL);
	
	private final String name;
	
	public Atom(Boolean bool) {
		this(bool.toString());
	}
	
	/**
	 * @param   name   the Atom's name (unquoted)
	 */
	public Atom(String name) {
		this.name = name;
	}

	
	public String getName() {
		return name;
	}


	@Override
	public boolean hasFunctor(Term nameTermObject, int arity) {
		return termEquals(nameTermObject) && arity == 0;
	}
	
	@Override
	public int hashCode() {
		return name.hashCode();
	}
	
	@Override
	public boolean equals(Object obj) {
		return (this == obj || (obj instanceof Atom && name.equals(((Atom)obj).name)));
	}
	
	/**
	 * Note: This method does not escape or quote the name
	 */
	@Override
	public String toString() {
		return getName();
	}
	
	@Override
	public String toString(PrologEngine prologEngine) {
		return prologEngine.escape(getName());
	}
	
	public boolean isBoolean() {
		return getName().equals(TRUE) || getName().equals(FAIL) || getName().equals(FALSE);
	}
	
	@Override
	public boolean isList() {
		return equals(EMPTY_LIST);
	}

	@Override
	public ListTerm asList() {
		if(isList())
			return new ListTerm();
		else
			throw new JpcException("The term " + this + " is not a list");
	}

	@Override
	public void accept(TermVisitor termVisitor) {
		termVisitor.visitAtom(this);
	}
	
	@Override
	public void read(TermContentHandler contentHandler) {
		contentHandler.startAtom(name);
	}
	
}
