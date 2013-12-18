package org.jpc.util;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;

import org.jpc.engine.prolog.driver.PrologEngineFactory;

public class JpcPropertiesFile {

	//Preferences file
	public static final String PREFERENCES_FILE = JpcPreferences.JPC_SHORT_NAME.toLowerCase() + ".properties";
	public static final String PREFERENCES_FILE_PACKAGE_PREFIX = JpcPreferences.JPC_SHORT_NAME.toLowerCase() + ".engine.";
	public static final String ROOT_ID = "rootPrologEngine";
	
	private final Properties properties;
	private final Map<String,Class<PrologEngineFactory<?>>> factoryClassesMap;
	
	public JpcPropertiesFile() throws FileNotFoundException {
		this(PREFERENCES_FILE);
	}
	
	public JpcPropertiesFile(String fileName) throws FileNotFoundException {
		this.properties = new Properties();
		InputStream is = getClass().getClassLoader().getResourceAsStream(fileName);
		try {
			properties.load(is);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		factoryClassesMap = new HashMap<>();
		for(Entry<Object,Object> entry : properties.entrySet()) {
			String propertyName = (String) entry.getKey();
			if(propertyName.startsWith(PREFERENCES_FILE_PACKAGE_PREFIX)) {
				String factoryClassName = (String) entry.getValue();
				Class<PrologEngineFactory<?>> factoryClass;
				try {
					factoryClass = (Class<PrologEngineFactory<?>>) Class.forName(factoryClassName);
				} catch (ClassNotFoundException e) {
					throw new RuntimeException(e);
				}
				String categoryName = propertyName.substring(PREFERENCES_FILE_PACKAGE_PREFIX.length());
				if(ROOT_ID.equals(categoryName))
					categoryName = "";
				factoryClassesMap.put(categoryName, factoryClass);
			}
		}
	}

	public Map<String, Class<PrologEngineFactory<?>>> getFactoryClassesMap() {
		return factoryClassesMap;
	}
	
}
