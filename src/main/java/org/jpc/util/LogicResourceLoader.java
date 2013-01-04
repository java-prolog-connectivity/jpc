package org.jpc.util;

import static java.util.Arrays.asList;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.jpc.JpcException;
import org.jpc.engine.logtalk.LogtalkEngine;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Atom;
import org.jpc.term.Term;
import org.minitoolbox.reflection.ReflectionUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Joiner;

public class LogicResourceLoader {
	private static Logger logger = LoggerFactory.getLogger(LogicResourceLoader.class);
	
	private PrologEngine logicEngine;
	private ResourceManager resourceManager;
	private ClassLoader[] classLoaders;
	
	public LogicResourceLoader(PrologEngine logicEngine, ClassLoader... classLoaders) {
		this(logicEngine, ResourceManager.getDefaultResourceManager(), classLoaders); //use the default resource manager if no one is provided
	}
	
	/**
	 * 
	 * @param logicEngine the logic engine where the resources will be loaded
	 * @param jpcPreferences the preferences defining (among other things) where logic tmp files will be copied before being loaded in the logic engine
	 */
	public LogicResourceLoader(PrologEngine logicEngine, ResourceManager resourceManager, ClassLoader... classLoaders) {
		this.logicEngine = logicEngine;
		this.resourceManager = resourceManager;
		this.classLoaders = classLoaders;
	}
	
	public boolean ensureLoaded(List<String> resources) {
		return logicEngine.ensureLoaded(resolveResources(resources));
	}
	
	public boolean ensureLoaded(String... resources) {
		return logicEngine.ensureLoaded(resolveResources(asList(resources)));
	}

	public boolean logtalkLoad(List<String> resources) {
		return logicEngine.asLogtalkEngine().logtalkLoad(resolveResources(resources));
	}
	
	public boolean logtalkLoad(String... resources) {
		return logicEngine.asLogtalkEngine().logtalkLoad(resolveResources(asList(resources)));
	}
	
	private List<Term> resolveResources(List<String> resources) {
		List<Term> resourceTerms = new ArrayList<>();
		for(String resourceString : resources) {
			resourceTerms.add(resolveResource(resourceString));
		}
		return resourceTerms;
	}
	
	private URL getResourceUrl(String resource) {
		URL url = null;
		Set<URL> urls = ReflectionUtil.getResources(resource, classLoaders);
		if(urls.size() > 0) {
			url = urls.iterator().next(); //take the first one
			if(urls.size() > 1) //in case there are many resources with the same name
				logger.warn("Resource " + resource + " found in multiple locations: " + Joiner.on(", ").join(urls) +". Chosen the one in: " + url);
		} 
		return url;
	}
	
	
	/**
	 * Given a resource name (possibly inside a jar) answers a Term that represents the resource in the file system. If the resource is not a library (i.e., with the form 'library(lib_name)'), it will be copied to a tmp location
	 * @param resource
	 * @return
	 */
	public Term resolveResource(String resource) {
		resource = resource.trim();
		if(resource.isEmpty())
			throw new JpcException("Invalid resource: empty string");
		Term resourceTerm = logicEngine.asResourceTerm(resource);
		if(resourceTerm instanceof Atom) { //it is not an alias but a concrete path
			//TODO maybe a smart default could be implemented when attempting to load a package/directory instead of a concrete file, for example, try to load a file in the directory called "load_all"
			if(resource.substring(resource.length()-1).equals("/")) {
				throw new JpcException("The resource to load is not a file");
			}
			URL resourceUrl = getResourceUrl(resource);
			if(resourceUrl == null) {
				Set<String> resourcesWithAnyExtension = ReflectionUtil.resourcesWithAnyExtension(resource, classLoaders);
				if(resourcesWithAnyExtension.isEmpty())
					throw new RuntimeException("Impossible to locate resource " + resource);
				resource = resourcesWithAnyExtension.iterator().next();
				if(resourcesWithAnyExtension.size() > 1)
					logger.warn("Multiple resources with the same name but different extensions: " + Joiner.on(", ").join(resourcesWithAnyExtension) + ". Trying with this: " + resource);
				resourceUrl = getResourceUrl(resource);
			}
			URL baseUrl = null;
			try {
				baseUrl = new URL(resourceUrl.toExternalForm().substring(0, resourceUrl.toExternalForm().lastIndexOf(resource)));
			} catch (MalformedURLException e) {
				throw new RuntimeException(e);
			}
			resourceTerm = resolveResource(resource, baseUrl);
		}
		return resourceTerm;
	}


	public Term resolveResource(String resourceName, URL url) {
		resourceManager.process(url); //will copy the logic files in the url in a temporary location (the resource manager remembers which urls have been processed before)
		String fileSystemPath = resourceManager.getResourcePath(resourceName, url);
		return new Atom(fileSystemPath);
	}
	
}
