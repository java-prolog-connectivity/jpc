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

	public static final String jsonConfigNoPackages = "{" + EngineConfigurationDeserializer.ENGINE_ID_PROPERTY + ": \"alias\", " + 
			EngineConfigurationDeserializer.FACTORY_CLASS_PROPERTY + ": \"" + JpcEngineDriver.class.getName() + "\"}";
	
	public static final String jsonConfigEmptyPackages = "{" + EngineConfigurationDeserializer.ENGINE_ID_PROPERTY + ": \"alias\", " + 
			EngineConfigurationDeserializer.CATEGORY_NAMES_PROPERTY + ": [], " + 
			EngineConfigurationDeserializer.FACTORY_CLASS_PROPERTY + ": \"" + JpcEngineDriver.class.getName() + "\"}";
	
	public static final String jsonConfigEmptyPackagesNoAlias = "{" + EngineConfigurationDeserializer.CATEGORY_NAMES_PROPERTY + ": [], " + //this is not a valid representation
			EngineConfigurationDeserializer.FACTORY_CLASS_PROPERTY + ": \"" + JpcEngineDriver.class.getName() + "\"}";
	
	public static final String jsonConfigNoAlias = "{" + EngineConfigurationDeserializer.CATEGORY_NAMES_PROPERTY + ": [\"p1\", \"p2\"], " + 
			EngineConfigurationDeserializer.FACTORY_CLASS_PROPERTY + ": \"" + JpcEngineDriver.class.getName() + "\"}";
	
	public static final String jsonConfigNoProfile = "{" + EngineConfigurationDeserializer.ENGINE_ID_PROPERTY + ": \"alias\", " + 
			EngineConfigurationDeserializer.CATEGORY_NAMES_PROPERTY + ": [\"p1\", \"p2\"], " + 
			EngineConfigurationDeserializer.FACTORY_CLASS_PROPERTY + ": \"" + JpcEngineDriver.class.getName() + "\"}";
	
	public static final String jsonConfigWithProfile = "{" + EngineConfigurationDeserializer.ENGINE_ID_PROPERTY + ": \"alias\", " + 
			EngineConfigurationDeserializer.CATEGORY_NAMES_PROPERTY + ": [\"p1\", \"p2\"], " + 
			EngineConfigurationDeserializer.FACTORY_CLASS_PROPERTY + ": \"" + JpcEngineDriver.class.getName() + "\", " +
			EngineConfigurationDeserializer.PROFILE_CLASS_PROPERTY + ": \"" + LogtalkEngineProfile.class.getName()+ "\"}";
	
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
