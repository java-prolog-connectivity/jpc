package org.jpc.term.visitor;

import org.jpc.term.AbstractVar;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Float;
import org.jpc.term.Integer;
import org.jpc.term.JRef;

public interface TermVisitor {

	void visitInteger(Integer integerTerm);
	void visitFloat(Float floatTerm);
	void visitVariable(AbstractVar variable);
	void visitAtom(Atom atom);
	void visitJRef(JRef<?> jRef);
	void visitSoftJRef(JRef<?> jRef);
	void visitWeakJRef(JRef<?> jRef);
	boolean visitCompound(Compound compound);
	
}
