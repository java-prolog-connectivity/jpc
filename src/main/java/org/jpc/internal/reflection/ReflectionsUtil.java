package org.jpc.internal.reflection;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashSet;
import java.util.Set;

/**
 * An utility class with methods related to the Google Reflections library
 * http://code.google.com/p/reflections/
 * 
 * @author sergioc
 *
 */
public class ReflectionsUtil {

	//TODO verify that this method is still necessary after doing what is said here: http://code.google.com/p/reflections/wiki/JBossIntegration
	/**
	 * This method is a workaround to the problem that the current version of reflections (at the moment of testing: 0.9.5 ) does not recognize JBoss URLs.
	 * TODO check if next versions of Reflections still have this problem, otherwise this method can be removed.
	 * @param urls the URLs to fix
	 * @return fixed URLs so they will be recognized by JBoss
	 */
	public static Set<URL> fixURLs(Iterable<URL> urls) {
	    Set<URL> results = new HashSet<URL>();
	    for (URL url : urls) {
	        String cleanURL = url.toString();
	        // Fix JBoss URLs
	        if (url.getProtocol().startsWith("vfszip")) {
	            cleanURL = cleanURL.replaceFirst("vfszip:", "file:");
	        } else if (url.getProtocol().startsWith("vfsfile")) {
	            cleanURL = cleanURL.replaceFirst("vfsfile:", "file:");
	        } else if(url.getProtocol().startsWith("vfs")) {//added by me
	              cleanURL = cleanURL.replaceFirst("vfs:", "file:");
	        } 
	        cleanURL = cleanURL.replaceFirst("\\.jar/", ".jar!/");
	        try {
	            results.add(new URL(cleanURL));
	        } catch (MalformedURLException ex) {
	        	throw new RuntimeException(ex);  // Shouldn't happen, but we can't do more to fix this URL.
	        }
	    }
	    return results;
	}

}
