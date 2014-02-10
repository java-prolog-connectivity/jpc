package org.jpc.term.visitor;

import org.jpc.term.AbstractVar;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.JRef;

public class DefaultTermVisitor implements TermVisitor {

	@Override
	public void visitInteger(IntegerTerm term) {
	}

	@Override
	public void visitFloat(FloatTerm term) {
	}

	@Override
	public void visitVariable(AbstractVar term) {
	}
	
	@Override
	public void visitAtom(Atom term) {
	}

	@Override
	public void visitJRef(JRef<?> jRef) {
	}
	
	@Override
	public void visitSoftJRef(JRef<?> jRef) {
	}
	
	@Override
	public void visitWeakJRef(JRef<?> jRef) {
	}
	
	@Override
	public boolean visitCompound(Compound term) {
		return true;
	}

}
