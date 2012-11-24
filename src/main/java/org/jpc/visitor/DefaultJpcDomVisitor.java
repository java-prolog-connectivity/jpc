package org.jpc.visitor;

import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.Variable;

public class DefaultJpcDomVisitor implements JpcDomVisitor {

	@Override
	public void visitInteger(IntegerTerm term) {
	}

	@Override
	public void visitFloat(FloatTerm term) {
	}

	@Override
	public void visitVariable(Variable term) {
	}
	
	@Override
	public void visitAtom(Atom term) {
	}

	@Override
	public boolean visitCompound(Compound term) {
		return true;
	}


}
