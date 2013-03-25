package org.jpc.engine.prolog.configuration;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.jpc.JpcException;
import org.jpc.JpcPreferences;
import org.jpc.engine.logtalk.LogtalkEngine;
import org.jpc.engine.prolog.PrologEngine;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class PrologEngineConfiguration {

	private static Logger logger = LoggerFactory.getLogger(PrologEngineConfiguration.class);
	
	protected JpcPreferences preferences;
	protected boolean enabled = true;
	private boolean configured = false;

	protected List<String> scope;
	protected boolean logtalkRequired = true;
	protected boolean logtalkLoaded = false;
	
	public PrologEngineConfiguration() {
		this(new JpcPreferences()); //default preferences
	}
	
	public PrologEngineConfiguration(JpcPreferences preferences) {
		this.preferences = preferences;
		scope = new ArrayList<String>();
		addScope(""); //the root package
	}


	public JpcPreferences getPreferences() {
		return preferences;
	}

	public void setPreferences(JpcPreferences preferences) {
		this.preferences = preferences;
	}

	public boolean isLogtalkLoaded() {
		return logtalkLoaded;
	}
	
	public boolean isLogtalkRequired() {
		return logtalkRequired;
	}

	public void setLogtalkRequired(boolean logtalkRequired) {
		this.logtalkRequired = logtalkRequired;
	}

	public boolean isEnabled() {
		return enabled;
	}
	
	public boolean isConfigured() {
		return configured;
	}

	public void setConfigured(boolean configured) {
		this.configured = configured;
	}

	public List<String> getScope() {
		return scope;
	}

	public void setScope(List<String> scope) {
		this.scope = scope;
	}

	public void setScope(String ...newScopes) {
		scope = new ArrayList<>(Arrays.asList(newScopes));
	}
	
	public void addScope(String ...newScopes) {
		scope.addAll(Arrays.asList(newScopes));
	}
	

	public PrologEngine createPrologEngine() {
		logger.info("Initializing logic engine");
		long startTime = System.nanoTime();
		if(!isConfigured()) {
			if(configure())
				setConfigured(true);
			else
				throw new JpcException("Impossible to configure the logic engine: " + getEngineName() + " using: " + getLibraryName());
		}
		PrologEngine newPrologEngine = basicCreatePrologEngine();
		if(isLogtalkRequired()) {
			String prologDialect = newPrologEngine.prologDialect();
			logger.info("Attempting to load logtalk in a " + prologDialect + " Prolog engine...");
			try {
				String logtalkIntegrationScript = preferences.logtalkIntegrationScript(prologDialect); //will throw an exception if a logtalk integration script cannot be found for a given engine
				logtalkLoaded = newPrologEngine.ensureLoaded(logtalkIntegrationScript);
			} catch(Exception ex) {
				logger.error(ex.getMessage());
			}
			
			if(logtalkLoaded) {
				logger.info("Logtalk loaded successfully");
			}
			else {
				logger.warn("Impossible to load Logtalk in the " + newPrologEngine.prologDialect() + " Logic Engine");
				logger.warn("Some features may not be available. If Logtalk is not required change the \"logtalkRequired\" property "+
						"in the configuration class " + this.getClass().getSimpleName());
				//throw new RuntimeException("Impossible to load Logtalk");
			}
			newPrologEngine.flushOutput();
		}
		onCreate(newPrologEngine);
		long endTime = System.nanoTime();
		long total = (endTime - startTime)/1000000;
		logger.info("A " + newPrologEngine.prologDialect() + " logic engine has been initialized in " + total + " milliseconds");
		return newPrologEngine;
	}

	public void onCreate(PrologEngine prologEngine) {
		//nothing by default, to be overridden if needed
	}
	
	public boolean configure() {
		//empty by default
		return true;
	}
	
	public String getDescription() {
		return "A " + getEngineName() + " Prolog engine connected by means of the " + getLibraryName() +" library";
	}
	
	/**
	 * 
	 * @return a instance of a Prolog engine. This instance has not been configured yet
	 */
	protected abstract PrologEngine basicCreatePrologEngine();
	
	public abstract String getLibraryName();
	
	public abstract String getEngineName();

	
}
