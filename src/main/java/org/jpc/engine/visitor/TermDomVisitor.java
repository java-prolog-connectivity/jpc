package org.jpc.engine.visitor;

import org.jpc.term.Compound;

public interface TermDomVisitor extends AbstractTermVisitor {

	public abstract boolean visitCompound(Compound compound);
	
}
