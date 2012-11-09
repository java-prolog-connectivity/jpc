package org.jpc.engine.visitor;

import org.jpc.term.Atom;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.Variable;

public class JpcAdapterVisitor implements JpcStreamingVisitor {

	protected JpcStreamingVisitor adaptee;
	
	public JpcAdapterVisitor(JpcStreamingVisitor adapteee) {
		this.adaptee = adapteee;
	}
	
	public JpcStreamingVisitor getAdaptee() {
		return adaptee;
	}
	
	@Override
	public void visitInteger(IntegerTerm integerTerm) {
		adaptee.visitInteger(integerTerm);
	}

	@Override
	public void visitFloat(FloatTerm floatTerm) {
		adaptee.visitFloat(floatTerm);
	}

	@Override
	public void visitVariable(Variable variable) {
		adaptee.visitVariable(variable);
	}

	@Override
	public void visitAtom(Atom atom) {
		adaptee.visitAtom(atom);
	}



	@Override
	public void visitCompound() {
		adaptee.visitCompound();
	}

	@Override
	public void visitCompoundName() {
		adaptee.visitCompoundName();
	}
	
	@Override
	public void endVisitCompoundName() {
		adaptee.endVisitCompoundName();
	}

	@Override
	public void visitCompoundArg() {
		adaptee.visitCompoundArg();
	}
	
	@Override
	public void endVisitCompoundArg() {
		adaptee.endVisitCompoundArg();
	}

	@Override
	public void endVisitCompound() {
		adaptee.endVisitCompound();
	}

}
