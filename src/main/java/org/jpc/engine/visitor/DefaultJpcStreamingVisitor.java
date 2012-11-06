package org.jpc.engine.visitor;

import org.jpc.term.Atom;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.Term;
import org.jpc.term.Variable;

public class DefaultJpcStreamingVisitor implements JpcStreamingVisitor {

	@Override
	public void visitInteger(IntegerTerm integerTerm) {
	}

	@Override
	public void visitFloat(FloatTerm floatTerm) {
	}

	@Override
	public void visitVariable(Variable variable) {
	}

	@Override
	public void visitAtom(Atom atom) {
	}

	@Override
	public void visitCompound() {
	}
	
	@Override
	public void visitCompoundName() {
	}

	@Override
	public void endVisitCompoundName() {
	}
	
	@Override
	public void visitCompoundArg() {
	}
	
	@Override
	public void endVisitCompoundArg() {
	}

	@Override
	public void endVisitCompound() {
	}



}
