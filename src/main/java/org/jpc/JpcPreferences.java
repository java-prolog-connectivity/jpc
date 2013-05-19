package org.jpc;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;

import java.io.File;
import java.util.Properties;

import org.minitoolbox.Preferences;

public class JpcPreferences extends Preferences {

	public static final String JPC_NAME = "The Java-Prolog Connectivity Library";
	public static final String JPC_SHORT_NAME = "JPC";
	public static final String JPC_BASE_PACKAGE = "org.jpc";
	
	public static final String JPC_VAR_PREFIX = "JPC_VAR_"; //prefix for all the variables exclusive to JPC (used by the library in certain cases, not directly written by the user)
	
	//Properties configuring the behaviour of JPC
	public final static String LOGTALK_HOME_ENV_VAR = "LOGTALKHOME";  //needed by the framework to find the integration scripts with Logtalk
	public final static String LOGTALK_USER_ENV_VAR = "LOGTALKUSER"; //logtalk user directory environment variable (used as a tmp directory if available)
	public final static String SYSTEM_TEMP_DIRECTORY_ENV_VAR = "tmp"; //the tmp environment variable
	
	
	
	public JpcPreferences() {
	}
	
	public JpcPreferences(Properties properties) {
		super(properties);
	}
	
//	public boolean isLogtalkSupported(String engineName) {
//		try {
//			logtalkIntegrationScriptOrThrow(engineName);
//			return true;
//		} catch(Exception e) {
//			return false;
//		}
//	}
	
	/**
	 * Answers a Logtalk integration script for a given Prolog engine name.
	 * This method should always return the valid path of an integration script.
	 * If it is not possible to find the integration script for a given engine, the method should thrown an exception detailing the reason of the failure.
	 * @param engineName the target engine name
	 * @return a full path of a Logtalk integration script for the given Prolog engine name.
	 */
	public String logtalkIntegrationScriptOrThrow(String engineName) {
		checkNotNull(engineName);
		checkArgument(!engineName.isEmpty());
		engineName = engineName.toLowerCase();
		File logtalkHomeFile = getLogtalkHomeOrThrow();
		String logtalkHome = logtalkHomeFile.getAbsolutePath();
		String scriptPath = logtalkHome + "/integration/";
		File scriptFolderFile = new File(scriptPath);
		if(!scriptFolderFile.exists())
			throw new RuntimeException("The " + LOGTALK_HOME_ENV_VAR + " environment variable does not seem to be pointing to a valid Logtalk installation. Please check that variable before attempting to load Logtalk.");
		String fileName = "logtalk_" + engineName + ".pl";
		scriptPath += fileName;
		File integrationScriptfile = new File(scriptPath);
		if(!integrationScriptfile.exists())
			throw new RuntimeException("The Logtalk installation at " + logtalkHome + " does not support the Prolog engine " + engineName);
		return scriptPath;
	}
	
	public String logtalkLibraryDirOrThrow() {
		File logtalkHomeFile = getLogtalkHomeOrThrow();
		return logtalkHomeFile.getAbsolutePath() + "/library/";
	}
	
	public File getLogtalkHomeOrThrow() {
		String logtalkHome = getVarOrThrow(LOGTALK_HOME_ENV_VAR);
		File logtalkHomeFile = new File(logtalkHome);
		if(!logtalkHomeFile.exists())
			throw new RuntimeException("Logtalk is not installed at " + logtalkHome + ". Please configure the " + LOGTALK_HOME_ENV_VAR + " environment variable or install Logtalk.");
		return logtalkHomeFile;
	}
	
	public String getTmpDirectory() {
		String tmp = getVar(LOGTALK_USER_ENV_VAR);
		if(tmp == null)
			tmp = getVar(SYSTEM_TEMP_DIRECTORY_ENV_VAR);
		return tmp;
	}
	
	public String getTmpSubdirectoryName() {
		return JPC_SHORT_NAME;
	}


}
