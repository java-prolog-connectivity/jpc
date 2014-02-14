package org.jpc.util.engine.supported;

public class Xsb extends EngineDescription {

	public static final String XSB = "XSB";
	
	@Override
	public String getName() {
		return XSB;
	}

	@Override
	public String getDescription() {
		return "XSB is a Logic Programming and Deductive Database system for Unix and Windows.";
	}

	@Override
	public String getLicenseUrl() {
		return "http://sourceforge.net/projects/xsb/";
	}

	@Override
	public String getSiteUrl() {
		return "http://xsb.sourceforge.net/";
	}
	
}
