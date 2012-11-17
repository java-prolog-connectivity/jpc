package org.jpc.term;

import org.jpc.JpcException;
import org.jpc.engine.visitor.AbstractJplVisitor;

/**
 * A class reifying a logic atom
 * @author scastro
 *
 */
public class Atom extends AbstractTerm {

	public static final Term TRUE_TERM = new Atom("true");
	public static final Term FALSE_TERM = new Atom("false");
	public static final Term EMPTY_LIST = new Atom("[]");
	
	protected final String name;
	
	/**
	 * @param   name   the Atom's name (unquoted)
	 */
	public Atom(String name) {
		this.name = name;
	}
	
	public String name() {
		return name;
	}


	@Override
	public boolean hasFunctor(TermConvertable nameTermObject, int arity) {
		return termEquals(nameTermObject) && arity == 0;
	}
	
	@Override
	public int hashCode() {
		return name.hashCode();
	}
	
	@Override
	public boolean equals(Object obj) {
		return (this == obj || (obj instanceof Atom && ((Atom)obj).canEquals(this) && name.equals(((Atom)obj).name)));
	}
	
	/**
	 * Any class overriding equals should override this method
	 * @param obj the object to compare
	 * @return whether this instance can equals the object sent as parameter
	 */
	public boolean canEquals(Object obj) {
		return obj instanceof Atom;
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
	public void accept(AbstractJplVisitor termVisitor) {
		termVisitor.visitAtom(this);
	}
	
	@Override
	public boolean isListTerm() {
		return equals(EMPTY_LIST);
	}

	@Override
	public ListTerm asListTerm() {
		if(isListTerm())
			return new EmptyList();
		else
			throw new JpcException("The term " + this + " is not a list");
	}
	
}
