package org.jpc.util.engine.supported;

import org.jpc.engine.dialect.Dialect;

public class Xsb extends EngineDescription {

	public static final String XSB_BIN_DIRECTORY_PROPERTY_NAME = "XSB_BIN_DIRECTORY"; //environment variable with the path to the XSB executable.
	private static final String EXECUTABLE_FILE_NAME = "xsb";

	@Override
	public String getName() {
		return getDialect().name();
	}

	@Override
	public Dialect getDialect() {
		return Dialect.XSB;
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
