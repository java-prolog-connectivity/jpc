package org.jpc.util.config;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Collections;

import org.jpc.engine.embedded.JpcEngineDriver;
import org.junit.Test;

public class EngineConfigurationDeserializerTest {

	public static final String jsonConfigNoPackages = "{" + EngineConfigurationDeserializer.ALIAS_PROPERTY_NAME + ": \"alias\", " + 
			EngineConfigurationDeserializer.ENGINE_FACTORY + ": \"" + JpcEngineDriver.class.getCanonicalName() + "\"}";
	
	public static final String jsonConfigEmptyPackages = "{" + EngineConfigurationDeserializer.ALIAS_PROPERTY_NAME + ": \"alias\", " + 
			EngineConfigurationDeserializer.PACKAGE_NAMES_PROPERTY_NAME + ": [], " + 
			EngineConfigurationDeserializer.ENGINE_FACTORY + ": \"" + JpcEngineDriver.class.getCanonicalName() + "\"}";
	
	public static final String jsonConfigEmptyPackagesNoAlias = "{" + EngineConfigurationDeserializer.PACKAGE_NAMES_PROPERTY_NAME + ": [], " + //this is not a valid representation
			EngineConfigurationDeserializer.ENGINE_FACTORY + ": \"" + JpcEngineDriver.class.getCanonicalName() + "\"}";
	
	public static final String jsonConfigNoAlias = "{" + EngineConfigurationDeserializer.PACKAGE_NAMES_PROPERTY_NAME + ": [\"p1\", \"p2\"], " + 
			EngineConfigurationDeserializer.ENGINE_FACTORY + ": \"" + JpcEngineDriver.class.getCanonicalName() + "\"}";
	
	public static final String jsonConfigAllData = "{" + EngineConfigurationDeserializer.ALIAS_PROPERTY_NAME + ": \"alias\", " + 
			EngineConfigurationDeserializer.PACKAGE_NAMES_PROPERTY_NAME + ": [\"p1\", \"p2\"], " + 
			EngineConfigurationDeserializer.ENGINE_FACTORY + ": \"" + JpcEngineDriver.class.getCanonicalName() + "\"}";
	
	@Test
	public void testJsonConfigNoPackages() {
		EngineConfiguration engineConfiguration = JpcConfigurationDeserializer.getContext().fromJson(jsonConfigNoPackages, EngineConfiguration.class);
		assertEquals("alias", engineConfiguration.getAlias());
		assertEquals(Collections.emptySet(), engineConfiguration.getPackageNames());
		assertTrue(engineConfiguration.getEngineFactory() instanceof JpcEngineDriver);
	}

	@Test
	public void testJsonConfigEmptyPackages() {
		EngineConfiguration engineConfiguration = JpcConfigurationDeserializer.getContext().fromJson(jsonConfigEmptyPackages, EngineConfiguration.class);
		assertEquals("alias", engineConfiguration.getAlias());
		assertEquals(Collections.emptySet(), engineConfiguration.getPackageNames());
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
		assertNull(engineConfiguration.getAlias());
		assertTrue(engineConfiguration.getPackageNames().contains("p1"));
		assertTrue(engineConfiguration.getPackageNames().contains("p2"));
		assertTrue(engineConfiguration.getEngineFactory() instanceof JpcEngineDriver);
	}
	
	@Test
	public void testJsonConfigAllData() {
		EngineConfiguration engineConfiguration = JpcConfigurationDeserializer.getContext().fromJson(jsonConfigAllData, EngineConfiguration.class);
		assertEquals("alias", engineConfiguration.getAlias());
		assertTrue(engineConfiguration.getPackageNames().contains("p1"));
		assertTrue(engineConfiguration.getPackageNames().contains("p2"));
		assertTrue(engineConfiguration.getEngineFactory() instanceof JpcEngineDriver);
	}
	
}
