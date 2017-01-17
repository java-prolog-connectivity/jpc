package org.jpc.term.visitor;

import org.jpc.term.AbstractVar;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Float;
import org.jpc.term.Integer;
import org.jpc.term.JRef;

public interface TermVisitor {

	public abstract void visitInteger(Integer integerTerm);
	public abstract void visitFloat(Float floatTerm);
	public abstract void visitVariable(AbstractVar variable);
	public abstract void visitAtom(Atom atom);
	public abstract void visitJRef(JRef<?> jRef);
	public abstract void visitSoftJRef(JRef<?> jRef);
	public abstract void visitWeakJRef(JRef<?> jRef);
	public abstract boolean visitCompound(Compound compound);
	
}
