package org.jpc;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;
import java.io.File;
import java.util.Properties;
import org.minitoolbox.Preferences;

public class JpcPreferences extends Preferences {

	public static final String JPC_NAME = "The Java Prolog Connectivity Library";
	public static final String JPC_SHORT_NAME = "JPC";
	
	//Properties configuring the framework behaviour
	public final static String LOGTALK_HOME_ENV = "LOGTALKHOME";  //needed by the framework to find the integration scripts
	public final static String LOGTALK_USER_ENV = "LOGTALKUSER"; //logtalk user directory environment variable
	public final static String SYSTEM_TEMP_DIRECTORY_ENV = "tmp";
	public final static String TEMP_SUBDIRECTORY_NAME_ENV = "TMP_SUBDIRECTORY";
	
	public JpcPreferences() {
		this(defaultJPCProperties());
	}
	
	public JpcPreferences(Properties properties) {
		super(properties);
	}
	
	public static Properties defaultJPCProperties() {
		Properties properties = new Properties();
		properties.put(TEMP_SUBDIRECTORY_NAME_ENV, JPC_SHORT_NAME);
		return properties;
	}
	
	public String getTmpDirectory() {
		String tmp = getVar(LOGTALK_USER_ENV);
		if(tmp == null)
			tmp = getVar(SYSTEM_TEMP_DIRECTORY_ENV);
		return tmp;
	}
	
	public String getTmpSubdirectoryName() {
		return getVar(TEMP_SUBDIRECTORY_NAME_ENV);
	}
	
	public String logtalkIntegrationScript(String engineName) {
		checkNotNull(engineName);
		checkArgument(!engineName.isEmpty());
		engineName = engineName.toLowerCase();
		String logtalkHome = getVarOrDie(LOGTALK_HOME_ENV);
		String scriptPath = logtalkHome + "/integration/";
		String fileName = "logtalk_" + engineName + ".pl";
		scriptPath += fileName;
		File file = new File(scriptPath);
		if(!file.exists())
			throw new RuntimeException("The Logtalk installation at " + logtalkHome + " does not support the Prolog engine " + engineName);
		return scriptPath;
	}

}
