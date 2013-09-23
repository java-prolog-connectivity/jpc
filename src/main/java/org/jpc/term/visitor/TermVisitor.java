package org.jpc.term.visitor;

import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.Var;

public interface TermVisitor {

	public abstract void visitInteger(IntegerTerm integerTerm);
	public abstract void visitFloat(FloatTerm floatTerm);
	public abstract void visitVariable(Var variable);
	public abstract void visitAtom(Atom atom);
	public abstract boolean visitCompound(Compound compound);
	
}
