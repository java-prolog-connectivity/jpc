package org.jpc.util.engine.supported;

import org.jpc.util.JpcPreferences;

public class JpcEmbedded extends EngineDescription {

	public static final String JPC_EMBEDDED_DIALECT = JpcPreferences.JPC_SHORT_NAME.toLowerCase();
	
	@Override
	public String getName() {
		return JpcPreferences.JPC_SHORT_NAME + " embedded";
	}

	@Override
	public String getDialect() {
		return JPC_EMBEDDED_DIALECT;
	}
	
	@Override
	public String getDescription() {
		return "The embedded " + JpcPreferences.JPC_SHORT_NAME + " Prolog engine.";
	}

	@Override
	public String getLicenseUrl() {
		return "https://github.com/java-prolog-connectivity/jpc/blob/master/LICENSE.txt";
	}

	@Override
	public String getSiteUrl() {
		return "https://github.com/java-prolog-connectivity/jpc/";
	}

}
