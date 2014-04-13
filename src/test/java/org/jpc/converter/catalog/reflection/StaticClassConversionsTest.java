package org.jpc.converter.catalog.reflection;

import static org.junit.Assert.assertEquals;

import org.jpc.Jpc;
import org.jpc.JpcBuilder;
import org.jpc.converter.catalog.reflection.reification.TypeConversionsTest.A.B.C;
import org.jpc.term.Compound;
import org.junit.Test;
import org.minitoolbox.reflection.StaticClass;

public class StaticClassConversionsTest {

	Jpc jpc = JpcBuilder.create().build();
	
	@Test
	public void testStaticClassConverter() {
		StaticClass staticClass = new StaticClass(String.class);
		Compound staticClassTerm = jpc.toTerm(staticClass);
		StaticClass staticClass2 = jpc.fromTerm(staticClassTerm);
		assertEquals(staticClass, staticClass2);
	}
	
	@Test
	public void testNestedStaticClassConverter() {
		StaticClass staticClass = new StaticClass(C.class);
		Compound staticClassTerm = jpc.toTerm(staticClass);
		StaticClass staticClass2 = jpc.fromTerm(staticClassTerm);
		assertEquals(staticClass, staticClass2);
	}

	@Test
	public void testPrimitiveStaticClassConverter() {
		StaticClass staticClass = new StaticClass(int.class);
		Compound staticClassTerm = jpc.toTerm(staticClass);
		StaticClass staticClass2 = jpc.fromTerm(staticClassTerm);
		assertEquals(staticClass, staticClass2);
	}
	
}
