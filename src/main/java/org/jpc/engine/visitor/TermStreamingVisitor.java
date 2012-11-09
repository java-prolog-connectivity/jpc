package org.jpc.engine.visitor;


public interface TermStreamingVisitor extends AbstractTermVisitor {

	public abstract void visitCompound();
	public abstract void visitCompoundName();
	public abstract void endVisitCompoundName();
	public abstract void visitCompoundArg();
	public abstract void endVisitCompoundArg();
	public abstract void endVisitCompound();
	
}
