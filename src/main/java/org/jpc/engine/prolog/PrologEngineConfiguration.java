package org.jpc.engine.prolog;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.jpc.JpcPreferences;
import org.jpc.engine.logtalk.LogtalkEngine;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class PrologEngineConfiguration {

	private static Logger logger = LoggerFactory.getLogger(PrologEngineConfiguration.class);
	
	protected JpcPreferences preferences;
	protected PrologEngine logicEngine;
	protected boolean enabled = true;
	private boolean configured = false;
	
	protected List<String> preloadedPrologResources;
	protected List<String> preloadedLogtalkResources;
	protected List<String> scope;
	protected boolean logtalkRequired = true;
	protected boolean logtalkLoaded = false;
	
	public PrologEngineConfiguration() {
		this(new JpcPreferences()); //default preferences
	}
	
	public PrologEngineConfiguration(JpcPreferences preferences) {
		this.preferences = preferences;
		preloadedPrologResources = new ArrayList<String>();
		preloadedLogtalkResources = new ArrayList<String>();
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

	public List<String> getPreloadedPrologResources() {
		return preloadedPrologResources;
	}

	public void setPreloadedPrologResources(List<String> preloadedPrologResources) {
		this.preloadedPrologResources = preloadedPrologResources;
	}

	public void addPreloadedPrologResources(String ...newPreloadedPrologResources) {
		preloadedPrologResources.addAll(Arrays.asList(newPreloadedPrologResources));
	}
	
	public List<String> getPreloadedLogtalkResources() {
		return preloadedLogtalkResources;
	}

	public void setPreloadedLogtalkResources(List<String> preloadedLogtalkResources) {
		this.preloadedLogtalkResources = preloadedLogtalkResources;
	}

	public void addPreloadedLogtalkResources(String ...newPreloadedLogtalkResources) {
		preloadedLogtalkResources.addAll(Arrays.asList(newPreloadedLogtalkResources));
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
	
	public PrologEngine getEngine() {
		if(logicEngine == null) {
			logger.info("Initializing logic engine");
			long startTime = System.nanoTime();
			if(!isConfigured()) {
				configure();
			}
			logicEngine = new PrologEngine(createBootstrapEngine());
			if(isLogtalkRequired()) {
				logger.info("Attempting to load logtalk ...");
				try {
					String prologDialect = logicEngine.prologDialect();
					String logtalkIntegrationScript = preferences.logtalkIntegrationScript(prologDialect); //will throw an exception if a logtalk integration script cannot be found for a given engine
					try {
						logtalkLoaded = logicEngine.ensureLoaded(logtalkIntegrationScript);
					} catch(Exception ex) {}
					if(!logtalkLoaded) {
						//throw new RuntimeException("Impossible to load Logtalk");
						logger.warn("Impossible to load Logtalk. Some features may not be available. If Logtalk is not required change the \"logtalkRequired\" property "+
								"for the configuration class " + this.getClass().getSimpleName());
					}
					else {
						logger.info("Logtalk loaded successfully");
					}
				} catch(Exception ex) {
					System.out.println(ex);
					logger.warn("Impossible to load Logtalk in the " + logicEngine.prologDialect() + " Logic Engine");
				}
			}
			loadPreloadedResources();
			long endTime = System.nanoTime();
			long total = (endTime - startTime)/1000000;
			logger.info("A " + logicEngine.prologDialect() + " logic engine has been initialized in " + total + " milliseconds");
		}
		return logicEngine;
	}

	private void loadPreloadedResources() {
		logicEngine.ensureLoaded(preloadedPrologResources.toArray(new String[]{}));
		if(isLogtalkRequired())
			logicEngine.asLogtalkEngine().logtalkLoad(preloadedLogtalkResources.toArray(new String[]{}));
	}

	public void configure() {
		//empty by default
	}
	
	/**
	 * The defaults configuration tasks of the engine
	 * @return
	 */
	protected abstract BootstrapPrologEngine createBootstrapEngine();

}
