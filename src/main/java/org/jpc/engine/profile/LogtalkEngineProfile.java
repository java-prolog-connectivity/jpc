package org.jpc.engine.profile;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.engine.prolog.driver.PrologEngineFactory;
import org.jpc.util.JpcPreferences;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LogtalkEngineProfile<T extends PrologEngine> extends PrologEngineProfile<T> {

	private static Logger logger = LoggerFactory.getLogger(LogtalkEngineProfile.class);
	private JpcPreferences preferences;
	
	public LogtalkEngineProfile(PrologEngineFactory<T> engineFactory) {
		this(engineFactory, new JpcPreferences());
	}
	
	public LogtalkEngineProfile(PrologEngineFactory<T> engineFactory, JpcPreferences preferences) {
		super(engineFactory);
		this.preferences = preferences;
	}

	public JpcPreferences getPreferences() {
		return preferences;
	}
	
	@Override
	public void onCreate(T newPrologEngine) {
		boolean logtalkLoaded = false;
		String prologDialect = newPrologEngine.prologDialect();
		logger.info("Attempting to load logtalk in a " + prologDialect + " Prolog engine...");
		
		long startTime = System.nanoTime();
		try {
			String logtalkIntegrationScript = preferences.logtalkIntegrationScriptOrThrow(prologDialect); //will throw an exception if a logtalk integration script cannot be found for a given engine
			logtalkLoaded = newPrologEngine.ensureLoaded(logtalkIntegrationScript);
		} catch(Exception ex) {
			logger.error(ex.getMessage());
		}
		newPrologEngine.flushOutput();
		if(logtalkLoaded) {
			logger.info("Logtalk loaded successfully");
			long endTime = System.nanoTime();
			long total = (endTime - startTime)/1000000;
			logger.info("Logtalk was configured in " + newPrologEngine.prologDialect() + " in " + total + " milliseconds");
		}
		else {
			logger.warn("Impossible to load Logtalk in the " + newPrologEngine.prologDialect() + " Logic Engine");
			logger.warn("Some features may not be available.");
			//throw new PrologEngineInitializationException("Impossible to load Logtalk in " + newPrologEngine.prologDialect());
		}
	}
	
}
