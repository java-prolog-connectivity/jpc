package org.jpc.visitor;

import org.jpc.term.Compound;

public interface JpcDomVisitor extends AbstractJpcVisitor {

	public abstract boolean visitCompound(Compound compound);
	
}
