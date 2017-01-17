package org.jpc.internal.reflection;

import static org.junit.Assert.assertEquals;

import java.util.Set;

import org.junit.Assert;
import org.junit.Test;

import com.google.common.base.Predicate;

public class ReflectionUtilTest {

	@Test
	public void testFindAllResources() {
		Predicate<String> isTxtFile = new Predicate<String>() {
			@Override
			public boolean apply(String s) {
				return s.endsWith(".txt");
			}
		};
		Set<String> resources;
		resources = ReflectionUtil.findResourcesInPackage("fixture", isTxtFile);
		assertEquals(3, resources.size());
		resources = ReflectionUtil.findResourcesInPackage("fixture", isTxtFile, false);
		assertEquals(2, resources.size());
	}

	@Test
	public void testFindResourceWithUnknownExtension() {
		Set<String> resources = ReflectionUtil.resourcesWithAnyExtension("fixture/file2");
		assertEquals(2, resources.size());
		resources = ReflectionUtil.resourcesWithAnyExtension("file2", "fixture", true);
		assertEquals(2, resources.size());
		resources = ReflectionUtil.resourcesWithAnyExtension("file0");//resources with name file0 (no folder)
		assertEquals(2, resources.size());
		resources = ReflectionUtil.resourcesWithAnyExtension("file0", "", true); //resources with name file0 in the root package, include subpackages
		assertEquals(3, resources.size());
		resources = ReflectionUtil.resourcesWithAnyExtension("file0", "", false); //resources with name file0 in the root package, do not include subpackages
		assertEquals(2, resources.size());
	}


	@Test
	public void testIsInterface() {
		Assert.assertTrue(ReflectionUtil.isInterface(FixtureReflectionUtil.Interface.class));
		Assert.assertFalse(ReflectionUtil.isInterface(FixtureReflectionUtil.Abstract.class));
		Assert.assertFalse(ReflectionUtil.isInterface(FixtureReflectionUtil.Concrete.class));
	}

	@Test
	public void testIsAbstract() {
		Assert.assertTrue(ReflectionUtil.isAbstract(FixtureReflectionUtil.Interface.class));
		Assert.assertTrue(ReflectionUtil.isAbstract(FixtureReflectionUtil.Abstract.class));
		Assert.assertTrue(ReflectionUtil.isAbstract(int.class)); //primitive classes are considered abstract
		Assert.assertFalse(ReflectionUtil.isAbstract(FixtureReflectionUtil.Concrete.class));
	}
	
}
