package org.jpc.util.config;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.util.Collections;
import java.util.Set;

import org.jpc.JpcException;
import org.jpc.engine.profile.PrologEngineProfile;
import org.jpc.engine.prolog.driver.PrologEngineFactory;
import org.jpc.engine.prolog.driver.PrologEngineFactoryMethod;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;

public class EngineConfigurationDeserializer implements JsonDeserializer<EngineConfiguration<?>> {
	
	public static final String ENGINE_ID_PROPERTY = "id";
	public static final String CATEGORY_NAMES_PROPERTY = "categoryNames";
	public static final String FACTORY_CLASS_PROPERTY = "factoryClass";
	public static final String FACTORY_METHOD_PROPERTY = "factoryMethod";
	public static final String PROFILE_CLASS_PROPERTY = "profileClass";
	
	@Override
	public EngineConfiguration<?> deserialize(JsonElement json, Type type, JsonDeserializationContext context) throws JsonParseException {
		JsonObject jObject = (JsonObject)json;
		String engineId = null;
		Set<String> categoryNames = Collections.emptySet();
		Class<? extends PrologEngineFactory<?>> factoryClass;
		Method factoryMethod = null;
		Object customFactory = null;
		PrologEngineFactory<?> engineFactory;
		
		JsonElement engineIdJson = jObject.get(ENGINE_ID_PROPERTY);
		if(engineIdJson != null)
			engineId = context.deserialize(engineIdJson, Object.class);
		
		JsonElement categoryNamesJson = jObject.get(CATEGORY_NAMES_PROPERTY);
		if(categoryNamesJson != null) {
			JsonArray categoryNamesJsonArray = categoryNamesJson.getAsJsonArray();
			categoryNames = context.deserialize(categoryNamesJsonArray, Set.class);
		}
		
		JsonElement factoryClassNameJson = jObject.get(FACTORY_CLASS_PROPERTY);
		if(factoryClassNameJson != null) {
			try {
				factoryClass = (Class<? extends PrologEngineFactory<?>>) Class.forName(factoryClassNameJson.getAsString());
				try {
					customFactory = factoryClass.newInstance();
				} catch (IllegalAccessException | InstantiationException e) {} //if it cannot be instantiated, a factory method should be present.
			} catch (ClassNotFoundException e) {
				throw new RuntimeException(e);
			}
		} else {
			throw new JpcException("Class name for engine factory should be specified.");
		}
		
		JsonElement factoryMethodNameJson = jObject.get(FACTORY_METHOD_PROPERTY);
		if(factoryMethodNameJson != null) {
			try {
				factoryMethod = factoryClass.getMethod(factoryMethodNameJson.getAsString());
			} catch (NoSuchMethodException | SecurityException e) {
				throw new RuntimeException(e);
			}
		}
		
		if(factoryMethod != null)
			engineFactory = new PrologEngineFactoryMethod<>(factoryMethod, customFactory);
		else { //a factory instance of PrologEngineFactory should have been instantiated.
			if(customFactory == null)
				throw new JpcException("Impossible to instantiate factory " + factoryClassNameJson.getAsString() + ".");
			if(!(customFactory instanceof PrologEngineFactory))
				throw new JpcException("Engine factory should be an instance of " + PrologEngineFactory.class.getCanonicalName() + ".");
			else
				engineFactory = (PrologEngineFactory<?>) customFactory;
		}

		JsonElement profileClassNameJson = jObject.get(PROFILE_CLASS_PROPERTY);
		if(profileClassNameJson != null) {
			try {
				Class<? extends PrologEngineProfile<?>> prologEngineProfileClass = (Class<? extends PrologEngineProfile<?>>) Class.forName(profileClassNameJson.getAsString());
				Constructor<? extends PrologEngineProfile<?>> prologEngineProfileConstructor = prologEngineProfileClass.getConstructor(new Class[]{PrologEngineFactory.class});
				engineFactory = prologEngineProfileConstructor.newInstance(engineFactory);
			}catch (ClassNotFoundException | InstantiationException | IllegalAccessException | NoSuchMethodException | SecurityException | IllegalArgumentException | InvocationTargetException e) {
				throw new RuntimeException(e);
			}
		}
		return new EngineConfiguration<>(engineId, categoryNames, engineFactory);
	}
	
}
