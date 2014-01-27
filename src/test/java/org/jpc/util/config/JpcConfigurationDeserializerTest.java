package org.jpc.util.config;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;

import java.util.Collections;

import org.junit.Test;

public class JpcConfigurationDeserializerTest {

	public static final String jsonEmptyJpcConfiguration = "{" + JpcConfigurationDeserializer.ENGINE_CONFIGURATIONS_PROPERTY_NAME + ": [" + "]}";
	
	public static final String jsonJpcConfigurationOneEngine = "{" + JpcConfigurationDeserializer.ENGINE_CONFIGURATIONS_PROPERTY_NAME + ": [" + EngineConfigurationDeserializerTest.jsonConfigNoPackages + "]}";
	
	public static final String jsonJpcConfigurationManyEngines = "{" + JpcConfigurationDeserializer.ENGINE_CONFIGURATIONS_PROPERTY_NAME + ": [" + EngineConfigurationDeserializerTest.jsonConfigNoPackages + ", " + 
			EngineConfigurationDeserializerTest.jsonConfigNoAlias + ", " +
			EngineConfigurationDeserializerTest.jsonConfigWithProfile + "]}";
	
	@Test
	public void testEmptyJpcConfiguration() {
		JpcConfiguration engineConfiguration = JpcConfigurationDeserializer.getContext().fromJson(jsonEmptyJpcConfiguration, JpcConfiguration.class);
		assertEquals(Collections.emptySet(), engineConfiguration.getEngineConfigurations());
	}
	
	@Test
	public void testJpcConfigurationOneEngine() {
		JpcConfiguration engineConfiguration = JpcConfigurationDeserializer.getContext().fromJson(jsonJpcConfigurationOneEngine, JpcConfiguration.class);
		assertEquals(1, engineConfiguration.getEngineConfigurations().size());
	}
	
	@Test
	public void testJpcConfigurationManyEngines() {
		JpcConfiguration engineConfiguration = JpcConfigurationDeserializer.getContext().fromJson(jsonJpcConfigurationManyEngines, JpcConfiguration.class);
		assertEquals(3, engineConfiguration.getEngineConfigurations().size());
	}
	
	@Test
	public void testEmptyString() {
		JpcConfiguration engineConfiguration = JpcConfigurationDeserializer.getContext().fromJson(" ", JpcConfiguration.class);
		assertNull(engineConfiguration);
		engineConfiguration = JpcConfigurationDeserializer.getContext().fromJson("{}", JpcConfiguration.class);
		assertNull(engineConfiguration);
		try {
			engineConfiguration = JpcConfigurationDeserializer.getContext().fromJson("[]", JpcConfiguration.class);
			fail();
		} catch(RuntimeException e) {}
		
	}
}
