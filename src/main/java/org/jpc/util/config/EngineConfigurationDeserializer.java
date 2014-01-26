package org.jpc.util.config;

import java.lang.reflect.Type;
import java.util.Collections;
import java.util.Set;

import org.jpc.JpcException;
import org.jpc.engine.prolog.driver.PrologEngineFactory;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;

public class EngineConfigurationDeserializer implements JsonDeserializer<EngineConfiguration> {
	
	public static final String ALIAS_PROPERTY_NAME = "alias";
	public static final String PACKAGE_NAMES_PROPERTY_NAME = "packageNames";
	public static final String ENGINE_FACTORY = "factory";
	
	
	@Override
	public EngineConfiguration deserialize(JsonElement json, Type type, JsonDeserializationContext context) throws JsonParseException {
		JsonObject jObject = (JsonObject)json;
		String alias = null;
		Set<String> packageNames = Collections.emptySet();
		PrologEngineFactory<?> engineFactory;
		
		JsonElement aliasJson = jObject.get(ALIAS_PROPERTY_NAME);
		if(aliasJson != null)
			alias = aliasJson.getAsString();
		
		JsonElement packageNamesJson = jObject.get(PACKAGE_NAMES_PROPERTY_NAME);
		if(packageNamesJson != null) {
			JsonArray packageNamesJsonArray = packageNamesJson.getAsJsonArray();
			packageNames = context.deserialize(packageNamesJsonArray, Set.class);
		}
		
		JsonElement classNameJson = jObject.get(ENGINE_FACTORY);
		if(classNameJson != null) {
			Class<? extends PrologEngineFactory<?>> prologEngineClass;
			try {
				prologEngineClass = (Class<? extends PrologEngineFactory<?>>) Class.forName(classNameJson.getAsString());
				engineFactory = prologEngineClass.newInstance();
			} catch (ClassNotFoundException | InstantiationException | IllegalAccessException e) {
				throw new RuntimeException(e);
			}
		} else {
			throw new JpcException("Class name for engine factory should be specified.");
		}
		return new EngineConfiguration(alias, packageNames, engineFactory);
	}
	
}
