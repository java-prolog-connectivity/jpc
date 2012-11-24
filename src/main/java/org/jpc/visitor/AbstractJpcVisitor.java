package org.jpc.visitor;

import org.jpc.term.Atom;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.Variable;

public interface AbstractJpcVisitor {

	public abstract void visitInteger(IntegerTerm integerTerm);
	public abstract void visitFloat(FloatTerm floatTerm);
	public abstract void visitVariable(Variable variable);
	public abstract void visitAtom(Atom atom);

}
