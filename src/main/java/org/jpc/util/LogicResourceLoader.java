package org.jpc.util;

import static java.util.Arrays.asList;

import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.jpc.JpcException;
import org.jpc.engine.LogicEngine;
import org.jpc.term.Atom;
import org.jpc.term.Term;
import org.minitoolbox.ReflectionUtil;
import org.reflections.util.ClasspathHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LogicResourceLoader {
	private static Logger logger = LoggerFactory.getLogger(LogicResourceLoader.class);
	
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
	
	/**
	 * Given a resource name (possibly inside a jar) answers a Term that represents the resource in the file system. If the resource is not a library, it will be copied to a tmp location
	 * @param resourceName
	 * @return
	 */
	public Term resolveResource(String resourceName) {
		resourceName = resourceName.trim();
		if(resourceName.isEmpty())
			throw new JpcException("Empty resource name");
		Term resourceTerm = logicEngine.asResourceTerm(resourceName);
		if(resourceTerm instanceof Atom) { //it is not an alias but a concrete path
			//TODO maybe a smart default could be implemented when attempting to load a package/directory instead of a concrete file, for example, try to load a file in the directory called "load_all"
			if(resourceName.substring(resourceName.length()-1).equals("/")) {
				throw new JpcException("The resource to load is not a file");
			}
			String[] splitted = resourceName.split("/");
			String fileResourceName = splitted[splitted.length-1];
			URL baseUrl;
			String parentPackage = resourceName.substring(0, resourceName.length() - fileResourceName.length());
			if(parentPackage.isEmpty() || parentPackage.equals("/")) //the resource is at the root.
				baseUrl = ReflectionUtil.getConsumerLibraryUrl(); //use the url of the user of the library
			else {
				Set<URL> urls = ClasspathHelper.forPackage(parentPackage); //TODO verify that it is working in applications with custom classloaders, such as Eclipse Plug-ins and web servers.
				if(urls.isEmpty())
					throw new JpcException("The package " + parentPackage + " cannot be located");
				if(urls.size() > 1) {
					logger.error("There are multiple urls containing the package " + parentPackage + ". Listing the URLs:"); 
					for(URL url: urls)
						logger.error(url.toString());
					throw new JpcException("There are multiple urls containing the package " + parentPackage + ". Listing the URLs:"); //TODO fix this finding the right URL in the set containing the resource instead of throwing an exception
				}
				baseUrl = urls.iterator().next(); //if no exception was thrown there is exactly one url in the set
			}
			resourceTerm = resolveResource(resourceName, baseUrl);
		}
		return resourceTerm;
	}
	
	public Term resolveResource(String resourceName, URL url) {
		resourceManager.process(url); //will copy the logic files in the url in a temporary location (the resource manager remembers which urls have been processed before)
		String fileSystemPath = resourceManager.getResourcePath(resourceName, url);
		return new Atom(fileSystemPath);
	}
	
	
	public static void main(String[] args) {
		String t = "a/";
		String[] sp = t.split("/");
		System.out.println(sp.length);
		System.out.println(sp[0]);
	}
}
