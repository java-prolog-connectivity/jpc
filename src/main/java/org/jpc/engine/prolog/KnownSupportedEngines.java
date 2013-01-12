package org.jpc.engine.prolog;

public class KnownSupportedEngines {

	public static final String SWI = "swi";
	
	public static final String YAP = "yap";
	
	public static final String XSB = "xsb";
	
	public static final String[] allEngines() {
		return new String[]{SWI, YAP, XSB};
	}
	
}
