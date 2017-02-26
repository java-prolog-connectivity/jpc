package org.jpc.mapping.converter.catalog.reflection;

import static org.junit.Assert.assertEquals;

import org.jpc.Jpc;
import org.jpc.JpcBuilder;
import org.jpc.mapping.converter.catalog.reflection.type.TypeConversionsTest.A.B.C;
import org.jpc.term.Compound;
import org.jpc.util.reification.ReflectiveClass;
import org.junit.Test;


public class ReflectiveClassConversionsTest {

	Jpc jpc = JpcBuilder.create().build();
	
	@Test
	public void testStaticClassConverter() {
		ReflectiveClass reflectiveClass = new ReflectiveClass(String.class);
		Compound staticClassTerm = jpc.toTerm(reflectiveClass);
		ReflectiveClass staticClass2 = jpc.fromTerm(staticClassTerm);
		assertEquals(reflectiveClass, staticClass2);
	}
	
	@Test
	public void testNestedStaticClassConverter() {
		ReflectiveClass reflectiveClass = new ReflectiveClass(C.class);
		Compound staticClassTerm = jpc.toTerm(reflectiveClass);
		ReflectiveClass staticClass2 = jpc.fromTerm(staticClassTerm);
		assertEquals(reflectiveClass, staticClass2);
	}

	@Test
	public void testPrimitiveStaticClassConverter() {
		ReflectiveClass reflectiveClass = new ReflectiveClass(int.class);
		Compound staticClassTerm = jpc.toTerm(reflectiveClass);
		ReflectiveClass staticClass2 = jpc.fromTerm(staticClassTerm);
		assertEquals(reflectiveClass, staticClass2);
	}
	
}
