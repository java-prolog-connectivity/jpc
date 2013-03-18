package org.jpc.instantiationmanager;

import static junit.framework.Assert.assertNotNull;
import static junit.framework.Assert.fail;

import java.util.ArrayList;
import java.util.List;

import org.jpc.converter.instantiation.DefaultInstantiationManager;
import org.jpc.converter.instantiation.InstantiationManager;
import org.junit.Test;

public class InstantiationManagerTest {

	@Test
	public void testDefaultInstantiationManager() {
		InstantiationManager im = new DefaultInstantiationManager();
		assertNotNull(im.instantiate(List.class));
		assertNotNull(im.instantiate(Iterable.class));
		assertNotNull(im.instantiate(ArrayList.class));
	}
	
	
	@Test
	public void testFailInstantiation() {
		InstantiationManager im = new InstantiationManager();
		try {
			List o = im.instantiate(List.class);
			fail();
		} catch(RuntimeException e) {
		}
	}

	
	@Test
	public void testSucceedInstantiation() {
		InstantiationManager im = new InstantiationManager();
		im.register(List.class);
		
		try {
			im.instantiate(List.class);
			fail();
		} catch(Exception e){}
		
		try {
			im.instantiate(Iterable.class);
			fail();
		} catch(Exception e){}

		assertNotNull(im.instantiate(ArrayList.class));

		im = new InstantiationManager();
		im.register(ArrayList.class);
		assertNotNull(im.instantiate(ArrayList.class));
		assertNotNull(im.instantiate(Iterable.class));
		assertNotNull(im.instantiate(ArrayList.class));
	}
}
