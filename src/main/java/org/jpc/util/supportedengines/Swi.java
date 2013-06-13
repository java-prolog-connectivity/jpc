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

	@Override
	public String getDescription() {
		return null;
	}

	@Override
	public String getLicenseUrl() {
		return null;
	}

	@Override
	public String getSiteUrl() {
		return null;
	}
	
	public String getExecutableFileName() {
		if(OsUtil.osIsOsX() || OsUtil.osIsWindows())
			return EXECUTABLE_FILE_NAME_WINDOWS_OSX;
		else
			return EXECUTABLE_FILE_NAME_LINUX;
	}

}
