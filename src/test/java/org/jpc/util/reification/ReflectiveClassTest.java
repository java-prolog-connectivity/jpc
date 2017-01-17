package org.jpc.util.reification;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.jpc.util.reification.ReflectiveClassTest.A.A1;
import org.jpc.util.reification.ReflectiveClassTest.B.B1;

public class ReflectiveClassTest {

	public static class A {
		public static class A1 {
			public static int m;
			public static Number m(Number n) {return n;}
		}
	}
	
	public static class B{
		public static class B1 extends A1 {
		}
	}


	@Test
	public void testEquals() {
		assertTrue(new ReflectiveClass(A1.class).equals(new ReflectiveClass(A1.class.getName())));
	}

	@Test
	public void testInvoke() {
		ReflectiveClass sc = new ReflectiveClass(A1.class);
		assertEquals(new Integer(1), sc.invoke("m", asList(1)));
	}

	@Test
	public void testDescendantInvoke() {
		ReflectiveClass sc = new ReflectiveClass(B1.class);
		assertEquals(new Integer(1), sc.invoke("m", asList(1)));
	}

	@Test
	public void testField() {
		ReflectiveClass sc = new ReflectiveClass(B1.class);
		assertEquals(new Integer(0), sc.getField("m"));
		sc.setField("m", 1);
		assertEquals(new Integer(1), sc.getField("m"));
	}
	
	@Test
	public void testNewInstance() {
		ReflectiveClass sc = new ReflectiveClass(String.class);
		assertEquals("hello", sc.newInstance(asList("hello")));
	}

}
