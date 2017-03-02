package org.jpc.term;

import static org.jpc.engine.prolog.PrologConstants.NIL_SYMBOL;

import org.jpc.engine.dialect.Dialect;
import org.jpc.engine.prolog.OperatorsContext;
import org.jpc.engine.prolog.PrologConstants;
import org.jpc.term.compiler.Environment;
import org.jpc.term.visitor.TermVisitor;
import org.jpc.util.PrologUtil;
import org.jpc.util.salt.TermContentHandler;

import java.util.function.Function;

/**
 * A class reifying a logic atom
 * @author scastro
 *
 */
public final class Atom extends Term {

	private final String name;
	private String escapedName; //lazily initialized.

	public static Atom atom(String name) {
		return new Atom(name);
	}

	public static Atom atom(Boolean bool) {
		return atom(bool.toString());
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
	public boolean hasFunctor(Functor functor) {
		return functor.getArity() == 0 && termEquals(functor.getName());
	}
	
	public boolean isBoolean() {
		return getName().equals(PrologConstants.TRUE) || getName().equals(PrologConstants.FAIL) || getName().equals(PrologConstants.FALSE);
	}
	
	@Override
	protected boolean basicIsList() {
		return name.equals(NIL_SYMBOL);
	}

	@Override
	public ListTerm asList() {
		if(isList())
			return new ListTerm();
		else
			throw new NotAListException(this);
	}

	@Override
	public boolean isGround() {
		return true;
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
	public String toEscapedString(Dialect dialect, OperatorsContext operatorsContext) {
		if(escapedName == null) {
			escapedName = name;
			if(!isList()) {
				escapedName = PrologUtil.escapeString(escapedName);
			}
		}
		return escapedName;
	}
	
	@Override
	public int hashCode() {
		return name.hashCode();
	}
	
	@Override
	public boolean equals(Object obj) {
		return (this == obj || (obj.getClass().equals(getClass()) && name.equals(((Atom)obj).name)));
	}
	

	@Override
	public Term preCompile(Environment env) {
		return new Atom(name.intern());
	}

	@Override
	public Term prepareForQuery(Environment env) {
		return new Atom(name.intern());
	}

	@Override
	public Term prepareForFrame(Environment env) {
		return this; //the Atom should be already compiled.
	}
	
}
