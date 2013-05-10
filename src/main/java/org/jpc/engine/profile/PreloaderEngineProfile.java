package org.jpc.engine.profile;

import java.util.List;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.engine.prolog.driver.PrologEngineFactory;
import org.jpc.resource.LogtalkResource;

public class PreloaderEngineProfile extends PrologEngineProfile {

	private List<String> absolutePaths;

	public PreloaderEngineProfile(PrologEngineFactory engineFactory, List<String> absolutePaths) {
		super(engineFactory);
		this.absolutePaths = absolutePaths;
	}

	@Override
	public void onCreate(PrologEngine newPrologEngine) {
		for(String absolutePath : absolutePaths) {
			if(LogtalkResource.hasLogtalkExtension(absolutePath))
				newPrologEngine.asLogtalkEngine().logtalkLoad(absolutePath);
			else
				newPrologEngine.ensureLoaded(absolutePath);
		}
	}

}
