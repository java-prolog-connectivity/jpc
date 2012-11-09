package org.jpc.engine.visitor;


public interface JpcStreamingVisitor extends AbstractJplVisitor {

	public abstract void visitCompound();
	public abstract void visitCompoundName();
	public abstract void endVisitCompoundName();
	public abstract void visitCompoundArg();
	public abstract void endVisitCompoundArg();
	public abstract void endVisitCompound();
	
}
