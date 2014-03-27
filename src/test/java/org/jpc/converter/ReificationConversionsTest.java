package org.jpc.converter;


import static org.junit.Assert.assertEquals;

import org.jpc.Jpc;
import org.jpc.JpcBuilder;
import org.jpc.converter.ReificationConversionsTest.A.B.C;
import org.jpc.term.Compound;
import org.junit.Test;
import org.minitoolbox.reflection.reification.StaticClass;

public class ReificationConversionsTest {

	public static class A {
		public static class B {
			public static class C {}
		}
	}
	
	@Test
	public void testStaticClassConverter() {
		StaticClass staticClass = new StaticClass(C.class);
		Jpc jpc = JpcBuilder.create().build();
		Compound staticClassTerm = jpc.toTerm(staticClass);
		StaticClass staticClass2 = jpc.fromTerm(staticClassTerm);
		assertEquals(staticClass, staticClass2);
	}

}
