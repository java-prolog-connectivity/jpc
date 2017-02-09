package org.jpc.util.engine.supported;

import org.jpc.engine.dialect.Dialect;
import org.jpc.util.JpcPreferences;

public class JpcEmbedded extends EngineDescription {

	@Override
	public String getName() {
		return getDialect().name() + " embedded";
	}

	@Override
	public Dialect getDialect() {
		return Dialect.JPC;
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
