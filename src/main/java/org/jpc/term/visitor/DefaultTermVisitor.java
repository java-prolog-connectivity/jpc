package org.jpc.term.visitor;

import org.jpc.term.AbstractVar;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Float;
import org.jpc.term.Integer;
import org.jpc.term.JRef;

public class DefaultTermVisitor implements TermVisitor {

	@Override
	public void visitInteger(Integer term) {
	}

	@Override
	public void visitFloat(Float term) {
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
