package org.jpc.util.reification;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.jpc.util.reification.ReflectiveObjectTest.A.A1;
import org.jpc.util.reification.ReflectiveObjectTest.B.B1;

public class ReflectiveObjectTest {

	public static class A {
		public static class A1 {
			public int m;
			public Number m(Number n) {return n;}
		}
	}
	
	public static class B{
		public static class B1 extends A1 {
		}
	}


	@Test
	public void testEquals() {
		Object o = new A1();
		assertTrue(new ReflectiveObject(o).equals(new ReflectiveObject(o)));
	}

	@Test
	public void testInvoke() {
		ReflectiveObject ro = new ReflectiveObject(new A1());
		assertEquals(new Integer(1), ro.invoke("m", asList(1)));
	}

	@Test
	public void testDescendantInvoke() {
		ReflectiveObject ro = new ReflectiveObject(new B1());
		assertEquals(new Integer(1), ro.invoke("m", asList(1)));
	}

	@Test
	public void testField() {
		ReflectiveObject ro = new ReflectiveObject(new B1());
		assertEquals(new Integer(0), ro.getField("m"));
		ro.setField("m", 1);
		assertEquals(new Integer(1), ro.getField("m"));
	}

}
