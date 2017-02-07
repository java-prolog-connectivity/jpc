package org.jpc.engine.prolog;


public abstract class PrologFlag extends Flag {

	public static Dialect DIALECT = new Dialect();
	
	public PrologFlag(String name) {
		super(name);
	}

	static class Dialect extends PrologFlag {
		public Dialect() {super("dialect");}
	} 
	

}
