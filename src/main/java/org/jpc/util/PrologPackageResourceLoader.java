package org.jpc.util;

import java.util.ArrayList;
import java.util.List;

import org.jpc.engine.LogicEngine;

public class PrologPackageResourceLoader {

	private LogicEngine logicEngine;
	
	public PrologPackageResourceLoader(LogicEngine logicEngine) {
		this.logicEngine = logicEngine;
	}
	
	public boolean ensureLoaded(List<String> resources) {
		return logicEngine.ensureLoaded(asFileSystemPaths(resources.toArray(new String[]{})));
	}
	
	public boolean ensureLoaded(String... resources) {
		return logicEngine.ensureLoaded(asFileSystemPaths(resources));
	}

	public boolean logtalkLoad(List<String> resources) {
		return logicEngine.logtalkLoad(asFileSystemPaths(resources.toArray(new String[]{})));
	}
	
	public boolean logtalkLoad(String... resources) {
		return logicEngine.logtalkLoad(asFileSystemPaths(resources));
	}
	
	private String[] asFileSystemPaths(String... resources) {
		List<String> fileSystemPaths = new ArrayList<>();
		for(String resource : resources) {
			String fileSystemPath = resource;
			//...
			fileSystemPaths.add(fileSystemPath);
		}
		return fileSystemPaths.toArray(new String[]{});
	}
	
	
	
}
