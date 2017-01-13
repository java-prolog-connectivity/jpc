package org.jpc.internal;

import org.jpc.internal.FixtureReflectionUtil.Abstract;
import org.jpc.internal.FixtureReflectionUtil.Concrete;
import org.jpc.internal.FixtureReflectionUtil.Interface;
import org.jpc.internal.reflection.ReflectionUtil;
import org.junit.Assert;
import org.junit.Test;

public class ReflectionUtilTest {

	@Test
	public void testIsInterface() {
		Assert.assertTrue(ReflectionUtil.isInterface(Interface.class));
		Assert.assertFalse(ReflectionUtil.isInterface(Abstract.class));
		Assert.assertFalse(ReflectionUtil.isInterface(Concrete.class));
	}

	@Test
	public void testIsAbstract() {
		Assert.assertTrue(ReflectionUtil.isAbstract(Interface.class));
		Assert.assertTrue(ReflectionUtil.isAbstract(Abstract.class));
		Assert.assertTrue(ReflectionUtil.isAbstract(int.class)); //primitive classes are considered abstract
		Assert.assertFalse(ReflectionUtil.isAbstract(Concrete.class));
	}
	
}
