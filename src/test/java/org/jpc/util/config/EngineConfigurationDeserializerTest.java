package org.jpc.util.config;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Collections;

import org.jpc.engine.embedded.JpcEngineDriver;
import org.jpc.engine.profile.LogtalkEngineProfile;
import org.junit.Test;

public class EngineConfigurationDeserializerTest {

	public static final String jsonConfigNoPackages = "{" + EngineConfigurationDeserializer.ID_PROPERTY_NAME + ": \"alias\", " + 
			EngineConfigurationDeserializer.ENGINE_FACTORY + ": \"" + JpcEngineDriver.class.getCanonicalName() + "\"}";
	
	public static final String jsonConfigEmptyPackages = "{" + EngineConfigurationDeserializer.ID_PROPERTY_NAME + ": \"alias\", " + 
			EngineConfigurationDeserializer.CATEGORY_NAMES_PROPERTY_NAME + ": [], " + 
			EngineConfigurationDeserializer.ENGINE_FACTORY + ": \"" + JpcEngineDriver.class.getCanonicalName() + "\"}";
	
	public static final String jsonConfigEmptyPackagesNoAlias = "{" + EngineConfigurationDeserializer.CATEGORY_NAMES_PROPERTY_NAME + ": [], " + //this is not a valid representation
			EngineConfigurationDeserializer.ENGINE_FACTORY + ": \"" + JpcEngineDriver.class.getCanonicalName() + "\"}";
	
	public static final String jsonConfigNoAlias = "{" + EngineConfigurationDeserializer.CATEGORY_NAMES_PROPERTY_NAME + ": [\"p1\", \"p2\"], " + 
			EngineConfigurationDeserializer.ENGINE_FACTORY + ": \"" + JpcEngineDriver.class.getCanonicalName() + "\"}";
	
	public static final String jsonConfigNoProfile = "{" + EngineConfigurationDeserializer.ID_PROPERTY_NAME + ": \"alias\", " + 
			EngineConfigurationDeserializer.CATEGORY_NAMES_PROPERTY_NAME + ": [\"p1\", \"p2\"], " + 
			EngineConfigurationDeserializer.ENGINE_FACTORY + ": \"" + JpcEngineDriver.class.getCanonicalName() + "\"}";
	
	public static final String jsonConfigWithProfile = "{" + EngineConfigurationDeserializer.ID_PROPERTY_NAME + ": \"alias\", " + 
			EngineConfigurationDeserializer.CATEGORY_NAMES_PROPERTY_NAME + ": [\"p1\", \"p2\"], " + 
			EngineConfigurationDeserializer.ENGINE_FACTORY + ": \"" + JpcEngineDriver.class.getCanonicalName() + "\", " +
			EngineConfigurationDeserializer.ENGINE_PROFILE + ": \"" + LogtalkEngineProfile.class.getCanonicalName()+ "\"}";
	
	@Test
	public void testJsonConfigNoPackages() {
		EngineConfiguration engineConfiguration = JpcConfigurationDeserializer.getContext().fromJson(jsonConfigNoPackages, EngineConfiguration.class);
		assertEquals("alias", engineConfiguration.getId());
		assertEquals(Collections.emptySet(), engineConfiguration.getCategoryNames());
		assertTrue(engineConfiguration.getEngineFactory() instanceof JpcEngineDriver);
	}

	@Test
	public void testJsonConfigEmptyPackages() {
		EngineConfiguration engineConfiguration = JpcConfigurationDeserializer.getContext().fromJson(jsonConfigEmptyPackages, EngineConfiguration.class);
		assertEquals("alias", engineConfiguration.getId());
		assertEquals(Collections.emptySet(), engineConfiguration.getCategoryNames());
		assertTrue(engineConfiguration.getEngineFactory() instanceof JpcEngineDriver);
	}
	
	@Test
	public void testJsonConfigEmptyPackagesNoAlias() {
		try {
			JpcConfigurationDeserializer.getContext().fromJson(jsonConfigEmptyPackagesNoAlias, EngineConfiguration.class);
			fail();
		} catch(RuntimeException e) {}
	}
	
	@Test
	public void testJsonConfigNoAlias() {
		EngineConfiguration engineConfiguration = JpcConfigurationDeserializer.getContext().fromJson(jsonConfigNoAlias, EngineConfiguration.class);
		assertNull(engineConfiguration.getId());
		assertTrue(engineConfiguration.getCategoryNames().contains("p1"));
		assertTrue(engineConfiguration.getCategoryNames().contains("p2"));
		assertTrue(engineConfiguration.getEngineFactory() instanceof JpcEngineDriver);
	}
	
	@Test
	public void testJsonConfigNoProfile() {
		EngineConfiguration engineConfiguration = JpcConfigurationDeserializer.getContext().fromJson(jsonConfigNoProfile, EngineConfiguration.class);
		assertEquals("alias", engineConfiguration.getId());
		assertTrue(engineConfiguration.getCategoryNames().contains("p1"));
		assertTrue(engineConfiguration.getCategoryNames().contains("p2"));
		assertTrue(engineConfiguration.getEngineFactory() instanceof JpcEngineDriver);
	}
	
	@Test
	public void testJsonConfigWithProfile() {
		EngineConfiguration engineConfiguration = JpcConfigurationDeserializer.getContext().fromJson(jsonConfigWithProfile, EngineConfiguration.class);
		assertEquals("alias", engineConfiguration.getId());
		assertTrue(engineConfiguration.getCategoryNames().contains("p1"));
		assertTrue(engineConfiguration.getCategoryNames().contains("p2"));
		assertTrue(engineConfiguration.getEngineFactory() instanceof LogtalkEngineProfile);
	}
	
}
