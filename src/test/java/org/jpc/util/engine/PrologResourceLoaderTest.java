package org.jpc.util.engine;

import static org.junit.Assert.*;

import java.net.URL;

import org.jpc.JpcException;
import org.jpc.util.JpcPreferences;
import org.jpc.util.ResourceManager;
import org.jpc.util.engine.PrologResourceLoader;

import static org.jpc.engine.provider.PrologEngineProviderManager.getPrologEngine;

import org.junit.Test;

public class PrologResourceLoaderTest {

	private ResourceManager resourceManager = new ResourceManager(new JpcPreferences()) {
		public boolean process(URL url) {
			return true;
		}
	};
	private PrologResourceLoader resourceLoader = new PrologResourceLoader(getPrologEngine(), resourceManager);
	
	@Test
	public void testResolveResource() {
		String dummyPackage = getClass().getPackage().getName();
		try {
			resourceLoader.resolveResource("");
			fail();
		} catch(JpcException e) {}//expected
		try {
			resourceLoader.resolveResource("/");
			fail();
		} catch(JpcException e) {}//expected
		try {
			resourceLoader.resolveResource("/" + dummyPackage + "/");
			fail();
		} catch(JpcException e) {}//expected
		try {
			resourceLoader.resolveResource(dummyPackage+"/");
			fail();
		} catch(JpcException e) {}//expected
//		resourceLoader.resolveResource(dummyPackage);
//		resourceLoader.resolveResource("/"+dummyPackage);
//		resourceLoader.resolveResource("/"+dummyPackage+"/file");
	}
}
