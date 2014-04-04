package org.jpc.util.engine.supported;

public class Xsb extends EngineDescription {

	public static final String XSB = "XSB";
	public static final String XSB_DIALECT = XSB.toLowerCase();
	public static final String XSB_BIN_DIRECTORY_PROPERTY_NAME = "XSB_BIN_DIRECTORY"; //environment variable with the path to the XSB executable.
	private static final String EXECUTABLE_FILE_NAME = "xsb";
	
	@Override
	public String getName() {
		return XSB;
	}

	@Override
	public String getDialect() {
		return XSB_DIALECT;
	}
	
	public String getExecutableFileName() {
		return EXECUTABLE_FILE_NAME;
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
