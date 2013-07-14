package org.jpc.util.supportedengines;

import org.minitoolbox.OsUtil;

public class Swi extends EngineDescription {

	public static final String SWI = "SWI";
	
	public static final String SWI_BIN_DIRECTORY_PROPERTY_NAME = "SWI_BIN_DIRECTORY";
	private static final String EXECUTABLE_FILE_NAME_WINDOWS_OSX = "swipl"; //executable for windows or osx
	private static final String EXECUTABLE_FILE_NAME_LINUX = "pl"; //executable for linux
	
	@Override
	public String getName() {
		return SWI;
	}

	public String getExecutableFileName() {
		if(OsUtil.osIsOsX() || OsUtil.osIsWindows())
			return EXECUTABLE_FILE_NAME_WINDOWS_OSX;
		else
			return EXECUTABLE_FILE_NAME_LINUX;
	}
	
	/**
	 * Source: http://en.wikipedia.org/wiki/SWI-Prolog
	 */
	@Override
	public String getDescription() {
		return "SWI-Prolog is an open source implementation of the programming language Prolog, "
				+ "commonly used for teaching and semantic web applications."; 
	}

	@Override
	public String getLicenseUrl() {
		return "http://www.swi-prolog.org/license.html";
	}

	@Override
	public String getSiteUrl() {
		return "http://www.swi-prolog.org/";
	}

}
