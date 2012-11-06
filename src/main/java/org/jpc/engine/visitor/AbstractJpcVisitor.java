package org.jpc.engine.visitor;

import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.Term;
import org.jpc.term.Variable;

public interface AbstractJpcVisitor {

	public abstract void visitInteger(IntegerTerm integerTerm);
	public abstract void visitFloat(FloatTerm floatTerm);
	public abstract void visitVariable(Variable variable);
	public abstract void visitAtom(Atom atom);

}
