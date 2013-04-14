package org.jpc.engine.prolog;


public abstract class PrologFlag extends Flag {

	public static Dialect DIALECT = new Dialect();
	
	public PrologFlag(String name) {
		super(name);
	}

	static class Dialect extends PrologFlag {
		public static final String SWI = "swi";
		public static final String YAP = "yap";
		public static final String XSB = "xsb";
		
		public Dialect() {super("dialect");}
	} 
	

}
