package org.jpc.util;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;
import static org.jpc.util.JpcPreferences.getEnvironmentVarOrThrow;

import java.io.File;

import org.jpc.JpcException;

public class LogtalkUtil {

	//Logtalk related properties
	public final static String LOGTALK_HOME_ENV_VAR = "LOGTALKHOME";  //needed by the framework to find the integration scripts with Logtalk
	public final static String LOGTALK_USER_ENV_VAR = "LOGTALKUSER"; //logtalk user directory environment variable (used as a tmp directory if available)
	
	/**
	 * Answers a Logtalk integration script for a given Prolog engine id.
	 * This method should always return the valid path of an integration script.
	 * If it is not possible to find the integration script for a given engine, the method should thrown an exception detailing the reason of the failure.
	 * @param engineName the target engine id
	 * @return a full path of a Logtalk integration script for the given Prolog engine id.
	 */
	public static String logtalkIntegrationScriptOrThrow(String engineName) {
		checkNotNull(engineName);
		checkArgument(!engineName.isEmpty());
		engineName = engineName.toLowerCase();
		File logtalkHomeDir = getLogtalkHomeOrThrow();
		String logtalkHome = logtalkHomeDir.getAbsolutePath();
		String scriptPath = logtalkHome + "/integration/";
		File scriptFolderFile = new File(scriptPath);
		if(!scriptFolderFile.exists())
			throw new JpcException("The " + LOGTALK_HOME_ENV_VAR + " environment variable does not seem to be pointing to a valid Logtalk installation. Please check that variable before attempting to load Logtalk.");
		String fileName = "logtalk_" + engineName + ".pl";
		scriptPath += fileName;
		File integrationScriptfile = new File(scriptPath);
		if(!integrationScriptfile.exists())
			throw new JpcException("The Logtalk installation at " + logtalkHome + " does not support the Prolog engine " + engineName);
		return scriptPath;
	}
	
	public static String logtalkLibraryDirOrThrow() {
		File logtalkHomeFile = getLogtalkHomeOrThrow();
		return logtalkHomeFile.getAbsolutePath() + "/library/";
	}
	
	public static File getLogtalkHomeOrThrow() {
		String logtalkHome = getEnvironmentVarOrThrow(LOGTALK_HOME_ENV_VAR);
		File logtalkHomeDir = new File(logtalkHome);
		if(!logtalkHomeDir.exists())
			throw new JpcException("Logtalk is not installed at " + logtalkHome + ". Please configure the " + LOGTALK_HOME_ENV_VAR + " environment variable or install Logtalk.");
		return logtalkHomeDir;
	}
	
	public static File getLogtalkUserDirOrThrow() {
		String logtalkUser = getEnvironmentVarOrThrow(LOGTALK_USER_ENV_VAR);
		File logtalkUserDir = new File(logtalkUser);
		if(!logtalkUserDir.exists())
			throw new JpcException("Logtalk user directory does not exist at " + logtalkUser + ". Please configure the " + LOGTALK_USER_ENV_VAR + " environment variable or install Logtalk.");
		return logtalkUserDir;
	}
	
}
