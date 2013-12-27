package org.jpc.term;

import static org.jpc.engine.prolog.PrologConstants.EMPTY_LIST_SYMBOL;
import static org.jpc.engine.prolog.PrologConstants.FAIL;
import static org.jpc.engine.prolog.PrologConstants.FALSE;
import static org.jpc.engine.prolog.PrologConstants.TRUE;

import java.util.regex.Matcher;

import org.jpc.JpcException;
import org.jpc.engine.prolog.OperatorsContext;
import org.jpc.salt.TermContentHandler;
import org.jpc.term.visitor.TermVisitor;

import com.google.common.base.Function;

/**
 * A class reifying a logic atom
 * @author scastro
 *
 */
public final class Atom extends Term {

	public static final Atom TRUE_TERM = new Atom(TRUE);
	public static final Atom FAIL_TERM = new Atom(FAIL); //preferring 'fail' over 'false' since 'fail' is ISO.
	public static final Atom EMPTY_LIST = new Atom(EMPTY_LIST_SYMBOL);
	
	private final String name;
	private final String escapedName; //for efficiency sake this is calculated once at instantiation time
	
	public Atom(Boolean bool) {
		this(bool.toString());
	}
	
	/**
	 * @param   id   the Atom's id (unquoted)
	 */
	public Atom(String name) {
		this.name = name;
		String escapedName = name;
		if(!isList()) {
			escapedName = escapedName.replaceAll("\\\\", Matcher.quoteReplacement("\\\\"));
			escapedName = escapedName.replaceAll("'", Matcher.quoteReplacement("''")); //escaping ' with \' does not work correctly in XSB, therefore it is escaped with the alternative ''
			escapedName = "'" + escapedName + "'";
		}
		this.escapedName = escapedName;
	}

	public String getName() {
		return name;
	}
	
	@Override
	public boolean hasFunctor(Functor functor) {
		return functor.getArity() == 0 && termEquals(functor.getName());
	}
	
	public boolean isBoolean() {
		return getName().equals(TRUE) || getName().equals(FAIL) || getName().equals(FALSE);
	}
	
	@Override
	protected boolean basicIsList() {
		return name.equals(EMPTY_LIST_SYMBOL);
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
	protected void basicRead(TermContentHandler contentHandler, Function<Term, Term> termExpander) {
		contentHandler.startAtom(name);
	}
	
	@Override
	public String toEscapedString() {
		return escapedName;
	}
	
	@Override
	protected int basicHashCode() {
		return name.hashCode();
	}
	
	@Override
	public boolean equals(Object obj) {
		return (this == obj || (obj instanceof Atom && name.equals(((Atom)obj).name)));
	}

	@Override
	public String toString(OperatorsContext operatorsContext) {
		return toString();
	}

}
