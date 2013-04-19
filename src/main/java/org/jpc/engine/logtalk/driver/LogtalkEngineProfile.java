package org.jpc.engine.logtalk.driver;

import org.jpc.JpcPreferences;
import org.jpc.engine.logtalk.LogtalkEngine;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.engine.prolog.driver.PrologEngineFactory;
import org.jpc.engine.prolog.driver.PrologEngineProfile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LogtalkEngineProfile extends PrologEngineProfile<LogtalkEngine> {

	private static Logger logger = LoggerFactory.getLogger(LogtalkEngineProfile.class);
	private JpcPreferences preferences;
	
	public LogtalkEngineProfile(PrologEngineFactory engineFactory) {
		this(engineFactory, new JpcPreferences());
	}
	
	public LogtalkEngineProfile(PrologEngineFactory engineFactory, JpcPreferences preferences) {
		super(engineFactory);
		this.preferences = preferences;
	}

	public JpcPreferences getPreferences() {
		return preferences;
	}
	
	@Override
	public LogtalkEngine basicCreatePrologEngine() {
		PrologEngine newPrologEngine = super.basicCreatePrologEngine();
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
		return new LogtalkEngine(newPrologEngine);
	}
	
}
