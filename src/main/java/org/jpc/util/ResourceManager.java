package org.jpc.util;

import static com.google.common.base.Preconditions.checkNotNull;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URL;
import java.util.HashSet;
import java.util.Set;

import org.jpc.JpcPreferences;
import org.jpc.resource.LogicResource;
import org.minitoolbox.io.FileUtil;
import org.reflections.Reflections;
import org.reflections.scanners.ResourcesScanner;
import org.reflections.util.ConfigurationBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Predicate;
import com.google.common.io.Resources;

/**
 * A utility class for copying all the logic resources in a given url to a tmp location
 * Answers the path of any given resource, given the base url of the resource
 * @author sergioc
 *
 */
public class ResourceManager {

	private static Logger logger = LoggerFactory.getLogger(ResourceManager.class);
	private JpcPreferences preferences;
	private static ResourceManager defaultResourceManager;

	public static ResourceManager getDefaultResourceManager() {
		if(defaultResourceManager == null)
			defaultResourceManager = new ResourceManager(new JpcPreferences()); //initialize the resource manager according to the default preferences
		return defaultResourceManager;
	}

	public static void setDefaultResourceManager(ResourceManager defaultResourceManager) {
		ResourceManager.defaultResourceManager = defaultResourceManager;
	}

	private final String tmpDirPath; // the root temporary directory 
	private final File jpcTmpDir; //a File object representing a folder in the tmp directory where logic files or similar resources can be unzipped if required

	private Set<URL> processedURLs; //remember which URLs have been processed already (i.e., tmp files have already been created for logic files in such url)
	//private JpcPreferences preferences;
	
	public ResourceManager(JpcPreferences preferences) {
		this.preferences = preferences;
		processedURLs = new HashSet<>();
		this.tmpDirPath = preferences.getTmpDirectory(); //the system tmp directory
		checkNotNull(tmpDirPath, "No tmp directory has been defined in the preferences");
		String tmpSubdirectoryPath = preferences.getTmpSubdirectoryName(); //the tmp subdirectory to be created in the tmp directory
		checkNotNull(tmpSubdirectoryPath, "No tmp subdirectory has been defined in the preferences");
		jpcTmpDir = new File(tmpDirPath, tmpSubdirectoryPath);
		jpcTmpDir.mkdirs(); //creating all the directories needed to locate the tmp logic files
	}
	
	public void reset() {
		processedURLs = new HashSet<>();
	}
	
	/**
	 * 
	 * @param url
	 * @return true if the url has already been processed (i.e., tmp files for all logic files have been copied in a tmp location). false otherwise
	 */
	public synchronized boolean hasBeenProcessed(URL url) {
		return processedURLs.contains(url);
	}
	
	/**
	 * Find all the logic files in the given url and copy them in a tmp folder
	 * @param url
	 * @return a boolean indicating if the url has been processed after this call
	 * if the URL was already processed before returns false
	 * If it is not possible to process the URL an exception will be launched
	 */
	public synchronized boolean process(URL url) {
		if(hasBeenProcessed(url))
			return false;
		//if(!isFileSystemDir(url)) { 
			try {
				/*
				 * This should be done if the logic files are in a jar or directly located in an exploded directory in the file system
				 * Even if the files are directly in the file system it does not mean they are accessible from the current execution path
				 * Referring always to the tmp directory 
				 */
				createTmpLogicFiles(url); 
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		//}
		processedURLs.add(url);
		return true;
	}


	
	/**
	 * Answers a convenient name of a tmp subdirectory for a given url
	 * @param url
	 * @return
	 */
	public File getTmpDir(URL url) {
		String tmpFolderPath = url.toExternalForm();
		tmpFolderPath = tmpFolderPath.replaceAll("[\\W]+", "_");
		tmpFolderPath = tmpFolderPath.replaceAll("[_]+$", ""); //drop the last _ (if any)
		return new File(jpcTmpDir, tmpFolderPath);
	}
	
	public void createTmpLogicFiles(URL url) throws IOException {
		File tmpDirForUrl = getTmpDir(url);
		if(tmpDirForUrl.exists()) {
			logger.debug("Deleting previous tmp directory: " + tmpDirForUrl.getAbsolutePath());
			FileUtil.deleteRecursively(tmpDirForUrl.toPath());
		}
		
		logger.debug("Creating tmp directory: " + tmpDirForUrl.getAbsolutePath());
		tmpDirForUrl.mkdir();
		
		Predicate<String> predicate = new Predicate<String>() {
			  public boolean apply(String string) {
				  return LogicResource.hasLogicExtension(string); //matching resources names ending  with the default Logtalk and Prolog extensions pl|P|lgt
			  }
			};
		copyResources(url, predicate, tmpDirForUrl);
	}

	
	public static void copyResources(URL url, Predicate<String> predicate, File destination) {// throws IOException {
		if(predicate == null)
			predicate = new Predicate<String>() {
				  public boolean apply(String string) {
					  return true; //no filter, so all the resources will be copied
				  }
		};
		
		Reflections reflections = new Reflections(new ConfigurationBuilder()
		.setUrls(url)
        .setScanners(new ResourcesScanner()));
		
		/*
		 * WARNING: the getResources method answers resources RELATIVE paths (relatives to the classpath from where they were found)
		 * If a file is created with this path (like with: new File(relativePath)) the path of such File object will be the current execution path + the relative path
		 * If the current execution path is not the base directory of the relative paths, this could lead to files having absolute paths pointing to non existing resources
		 */
		Set<String> resourcePaths = reflections.getResources(predicate);  //in case a complex predicate is needed
		
		for(String resourcePath : resourcePaths) {
			logger.debug("Copying resource to tmp location: " + resourcePath);
			File fileToCreate = new File(destination, resourcePath);
			fileToCreate.getParentFile().mkdirs();
			
			try(FileOutputStream fos = new FileOutputStream(fileToCreate)) {
				URL urlResources = new URL(url, resourcePath);
				Resources.copy(urlResources, fos);
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		}
	}

	
	/**
	 * 
	 * @param url
	 * @return
	 */
	private File basePath(URL url) {
		/*
		if(isFileSystemDir(url)) 
			return new File(url.getFile());
		else*/
			return getTmpDir(url); //always consider the base path the temp directory given the url sent as parameter
	}

	public String getResourcePath(String resource, URL url) {
		File baseDirectory = basePath(url);
		File resourceFile = new File(baseDirectory, resource);
		return resourceFile.getAbsolutePath();
	}
	


}

