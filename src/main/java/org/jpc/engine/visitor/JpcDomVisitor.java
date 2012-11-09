package org.jpc.engine.visitor;

import org.jpc.term.Compound;

public interface JpcDomVisitor extends AbstractJplVisitor {

	public abstract boolean visitCompound(Compound compound);
	
}
