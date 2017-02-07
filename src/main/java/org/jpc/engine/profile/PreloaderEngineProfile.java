package org.jpc.engine.profile;

import java.util.List;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.engine.prolog.driver.PrologEngineFactory;
import org.jpc.util.resource.LogtalkResource;

public class PreloaderEngineProfile<T extends PrologEngine> extends PrologEngineProfile<T> {

	private List<String> absolutePaths;

	public PreloaderEngineProfile(PrologEngineFactory<T> engineFactory, List<String> absolutePaths) {
		super(engineFactory);
		this.absolutePaths = absolutePaths;
	}

	@Override
	public void onCreate(T newPrologEngine) {
		for(String absolutePath : absolutePaths) {
			if(LogtalkResource.hasLogtalkExtension(absolutePath))
				newPrologEngine.asLogtalkEngine().logtalkLoad(absolutePath);
			else
				newPrologEngine.ensureLoaded(absolutePath);
		}
	}

}
