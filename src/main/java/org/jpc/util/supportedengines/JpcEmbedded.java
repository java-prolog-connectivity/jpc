package org.jpc.util.supportedengines;

import org.jpc.util.JpcPreferences;

public class JpcEmbedded extends EngineDescription {

	@Override
	public String getName() {
		return JpcPreferences.JPC_SHORT_NAME + " embedded";
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
