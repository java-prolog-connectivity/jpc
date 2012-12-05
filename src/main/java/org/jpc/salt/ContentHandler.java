package org.jpc.salt;



public interface ContentHandler {

	public abstract void startIntegerTerm(long value);
	public abstract void startFloatTerm(double value);
	public abstract void startVariable(String name);
	public abstract void startAtom(String name);
	
	public abstract void startCompoundName();
	public abstract void startCompoundArgs();
	public abstract void startCompoundArg();
	public abstract void endCompoundArg();
	public abstract void endCompoundArgs();
	
}
