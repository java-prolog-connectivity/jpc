package org.jpc.util.config;

import java.lang.reflect.Type;
import java.util.Set;

import com.google.common.reflect.TypeToken;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;

public class JpcConfigurationDeserializer implements JsonDeserializer<JpcConfiguration> {

	public static final String ENGINE_CONFIGURATIONS_PROPERTY_NAME = "engines";
	
	private static final Gson gson;
	
	static {
		GsonBuilder builder = new GsonBuilder();
		builder.registerTypeAdapter(EngineConfiguration.class, new EngineConfigurationDeserializer());
		builder.registerTypeAdapter(JpcConfiguration.class, new JpcConfigurationDeserializer());
		gson = builder.create();
	}
	
	public static Gson getContext() {
		return gson;
	}
	
	public static JpcConfiguration fromJson(String json) {
		return gson.fromJson(json, JpcConfiguration.class);
	}

	@Override
	public JpcConfiguration deserialize(JsonElement json, Type type, JsonDeserializationContext context) throws JsonParseException {
		JsonObject jObject = (JsonObject)json;
		JpcConfiguration jpcConfiguration = null;
		JsonElement engineConfigurationsJson = jObject.get(ENGINE_CONFIGURATIONS_PROPERTY_NAME);
		if(engineConfigurationsJson != null) {
			Type collectionType = new TypeToken<Set<EngineConfiguration>>(){}.getType();
			Set<EngineConfiguration> engineConfigurations = context.deserialize(engineConfigurationsJson, collectionType);
			jpcConfiguration = new JpcConfiguration(engineConfigurations);
		}
		return jpcConfiguration;
	}

}
