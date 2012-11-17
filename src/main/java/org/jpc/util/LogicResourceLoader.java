package org.jpc.util;

import static java.util.Arrays.asList;

import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.jpc.engine.LogicEngine;
import org.jpc.term.Atom;
import org.jpc.term.Term;
import org.reflections.util.ClasspathHelper;

public class LogicResourceLoader {

	private LogicEngine logicEngine;
	private ResourceManager resourceManager;
	
	public LogicResourceLoader(LogicEngine logicEngine) {
		this(logicEngine, ResourceManager.getDefaultResourceManager()); //use the default resource manager if no one is provided
	}
	
	/**
	 * 
	 * @param logicEngine the logic engine where the resources will be loaded
	 * @param jpcPreferences the preferences defining (among other things) where logic tmp files will be copied before being loaded in the logic engine
	 */
	public LogicResourceLoader(LogicEngine logicEngine, ResourceManager resourceManager) {
		this.logicEngine = logicEngine;
		this.resourceManager = resourceManager;
	}
	
	public boolean ensureLoaded(List<String> resources) {
		return logicEngine.ensureLoaded(resolveResources(resources));
	}
	
	public boolean ensureLoaded(String... resources) {
		return logicEngine.ensureLoaded(resolveResources(asList(resources)));
	}

	public boolean logtalkLoad(List<String> resources) {
		return logicEngine.logtalkLoad(resolveResources(resources));
	}
	
	public boolean logtalkLoad(String... resources) {
		return logicEngine.logtalkLoad(resolveResources(asList(resources)));
	}
	
	private List<Term> resolveResources(List<String> resourcesString) {
		List<Term> resources = new ArrayList<>();
		for(String resourceString : resourcesString) {
			resources.add(resolveResource(resourceString));
		}
		return resources;
	}
	
	public Term resolveResource(String resourceName) {
		resourceName = resourceName.trim();
		if(resourceName.isEmpty())
			throw new RuntimeException("Empty resource name");
		Term resourceTerm = logicEngine.asResourceTerm(resourceName);
		if(resourceTerm instanceof Atom) { //it is not an alias but a concrete path
			//TODO maybe a smart default could be implemented when attempting to load a package/directory instead of a concrete file, for example, try to load a file in the directory called "load_all"
			if(resourceName.substring(resourceName.length()-1).equals("/")) {
				throw new RuntimeException("The resource to load is not a file");
			}
				
			String[] splitted = resourceName.split("/");
			String fileResourceName = splitted[splitted.length-1];
			String parent = resourceName.substring(0, resourceName.length() - fileResourceName.length());
			Set<URL> urls = ClasspathHelper.forPackage(parent); //TODO verify that it is working in applications with custom classloaders, such as Eclipse Plug-ins and web servers.
			if(urls.isEmpty())
				throw new RuntimeException("The resource cannot be located");
			if(urls.size() > 1)
				throw new RuntimeException();//there are multiple urls having the same package. TODO fix this finding the right URL in the set containing the resource
			URL url = urls.iterator().next(); //if no exception was thrown there is exactly one url in the set
			resourceManager.process(url); //will copy the logic files in the url in a temporary location (will remember which urls have been processed before)
			String fileSystemPath = resourceManager.getResourcePath(resourceName, url);
			resourceTerm = new Atom(fileSystemPath);
		}
		return resourceTerm;
	}
	
	public static void main(String[] args) {
		String t = "a/";
		String[] sp = t.split("/");
		System.out.println(sp.length);
		System.out.println(sp[0]);
	}
}
