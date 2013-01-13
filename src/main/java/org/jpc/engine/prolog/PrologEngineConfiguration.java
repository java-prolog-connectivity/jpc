package org.jpc.engine.prolog;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.jpc.engine.logtalk.LogtalkEngine;
import org.jpc.util.JpcPreferences;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class PrologEngineConfiguration {

	private static Logger logger = LoggerFactory.getLogger(PrologEngineConfiguration.class);
	
	protected JpcPreferences preferences;
	private PrologEngine cachedPrologEngine;
	protected boolean enabled = true;
	private boolean configured = false;
	
	protected List<String> preloadedPrologResources;
	protected List<String> preloadedLogtalkResources;
	protected List<String> scope;
	protected boolean logtalkRequired = true;
	protected boolean logtalkLoaded = false;
	private boolean caching;
	
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

	public boolean isCaching() {
		return caching;
	}

	public void setCaching(boolean caching) {
		this.caching = caching;
		if(!caching)
			cachedPrologEngine = null;
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
	
	public PrologEngine createPrologEngine() {
		logger.info("Initializing logic engine");
		long startTime = System.nanoTime();
		if(!isConfigured()) {
			configure();
		}
		PrologEngine newPrologEngine = new PrologEngine(createBootstrapEngine());
		if(isLogtalkRequired()) {
			logger.info("Attempting to load logtalk ...");
			try {
				String prologDialect = newPrologEngine.prologDialect();
				String logtalkIntegrationScript = preferences.logtalkIntegrationScript(prologDialect); //will throw an exception if a logtalk integration script cannot be found for a given engine
				try {
					logtalkLoaded = newPrologEngine.ensureLoaded(logtalkIntegrationScript);
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
				logger.warn("Impossible to load Logtalk in the " + newPrologEngine.prologDialect() + " Logic Engine");
			}
		}
		loadPreloadedResources(newPrologEngine);
		long endTime = System.nanoTime();
		long total = (endTime - startTime)/1000000;
		logger.info("A " + newPrologEngine.prologDialect() + " logic engine has been initialized in " + total + " milliseconds");
		if(caching)
			cachedPrologEngine = newPrologEngine;
		return newPrologEngine;
	}
	
	public PrologEngine getEngine() {
		PrologEngine prologEngine = null;
		if(caching) {
			prologEngine = cachedPrologEngine;
		}
		if(prologEngine == null) {
			prologEngine = createPrologEngine();
		}
		return prologEngine;
	}

	private void loadPreloadedResources(PrologEngine prologEngine) {
		prologEngine.ensureLoaded(preloadedPrologResources.toArray(new String[]{}));
		if(isLogtalkRequired())
			prologEngine.asLogtalkEngine().logtalkLoad(preloadedLogtalkResources.toArray(new String[]{}));
	}

	public void configure() {
		//empty by default
	}
	
	/**
	 * The defaults configuration tasks of the engine
	 * @return
	 */
	protected abstract BootstrapPrologEngine createBootstrapEngine();
	
	public abstract String getLibraryName();
	
	public abstract String getEngineName();

}
