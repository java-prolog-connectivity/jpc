package org.jpc.util.config;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Type;
import java.util.Collections;
import java.util.Set;

import org.jpc.JpcException;
import org.jpc.engine.profile.PrologEngineProfile;
import org.jpc.engine.prolog.driver.PrologEngineFactory;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;

public class EngineConfigurationDeserializer implements JsonDeserializer<EngineConfiguration<?>> {
	
	public static final String ID_PROPERTY_NAME = "id";
	public static final String PACKAGE_NAMES_PROPERTY_NAME = "packageNames";
	public static final String ENGINE_FACTORY = "factory";
	public static final String ENGINE_PROFILE = "profile";
	
	@Override
	public EngineConfiguration<?> deserialize(JsonElement json, Type type, JsonDeserializationContext context) throws JsonParseException {
		JsonObject jObject = (JsonObject)json;
		String engineId = null;
		Set<String> packageNames = Collections.emptySet();
		PrologEngineFactory<?> engineFactory;
		
		JsonElement engineIdJson = jObject.get(ID_PROPERTY_NAME);
		if(engineIdJson != null)
			engineId = context.deserialize(engineIdJson, Object.class);
		
		JsonElement packageNamesJson = jObject.get(PACKAGE_NAMES_PROPERTY_NAME);
		if(packageNamesJson != null) {
			JsonArray packageNamesJsonArray = packageNamesJson.getAsJsonArray();
			packageNames = context.deserialize(packageNamesJsonArray, Set.class);
		}
		
		JsonElement factoryClassNameJson = jObject.get(ENGINE_FACTORY);
		if(factoryClassNameJson != null) {
			try {
				Class<? extends PrologEngineFactory<?>> prologEngineClass = (Class<? extends PrologEngineFactory<?>>) Class.forName(factoryClassNameJson.getAsString());
				engineFactory = prologEngineClass.newInstance();
			} catch (ClassNotFoundException | InstantiationException | IllegalAccessException e) {
				throw new RuntimeException(e);
			}
		} else {
			throw new JpcException("Class name for engine factory should be specified.");
		}
		
		JsonElement profileClassNameJson = jObject.get(ENGINE_PROFILE);
		if(profileClassNameJson != null) {
			try {
				Class<? extends PrologEngineProfile<?>> prologEngineProfileClass = (Class<? extends PrologEngineProfile<?>>) Class.forName(profileClassNameJson.getAsString());
				Constructor<? extends PrologEngineProfile<?>> prologEngineProfileConstructor = prologEngineProfileClass.getConstructor(new Class[]{PrologEngineFactory.class});
				engineFactory = prologEngineProfileConstructor.newInstance(engineFactory);
			}catch (ClassNotFoundException | InstantiationException | IllegalAccessException | NoSuchMethodException | SecurityException | IllegalArgumentException | InvocationTargetException e) {
				throw new RuntimeException(e);
			}
		}
		return new EngineConfiguration<>(engineId, packageNames, engineFactory);
	}
	
}
