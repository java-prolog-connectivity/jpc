package org.jpc.mapping.converter.catalog.reflection.type;


import static org.jpc.internal.reflection.ReflectionUtil.genericArrayType;
import static org.junit.Assert.assertEquals;

import java.lang.reflect.Type;
import java.util.List;

import org.jpc.Jpc;
import org.jpc.JpcBuilder;
import org.jpc.mapping.converter.catalog.reflection.type.TypeConversionsTest.A.B.C;
import org.jpc.term.Compound;
import org.junit.Test;

import com.google.common.reflect.TypeToken;

public class TypeConversionsTest {

	Jpc jpc = JpcBuilder.create().build();
	
	public static class A {
		public static class B {
			public static class C {}
		}
	}
	

	@Test
	public void testClassConverter() {
		Class<?> class1 = String.class;
		Compound classTerm = jpc.toTerm(class1);
		Class<?> class2 = jpc.fromTerm(classTerm);
		assertEquals(class1, class2);
	}
	
	@Test
	public void testNestedClassConverter() {
		Class<?> class1 = C.class;
		Compound classTerm = jpc.toTerm(class1);
		Class<?> class2 = jpc.fromTerm(classTerm);
		assertEquals(class1, class2);
	}

	@Test
	public void testPrimitiveClassConverter() {
		Class<?> class1 = int.class;
		Compound classTerm = jpc.toTerm(class1);
		Class<?> class2 = jpc.fromTerm(classTerm);
		assertEquals(class1, class2);
	}

	@Test
	public void testRawClassConverter() {
		Class<?> class1 = List.class;
		Compound classTerm = jpc.toTerm(class1);
		Class<?> class2 = jpc.fromTerm(classTerm);
		assertEquals(class1, class2);
	}
	
	@Test
	public void testParameterizedTypeConverter() {
		Type type1 = new TypeToken<List<String>>(){}.getType();
		Compound typeTerm = jpc.toTerm(type1);
		Type type2 = jpc.fromTerm(typeTerm);
		assertEquals(type1, type2);
	}
	
	@Test
	public void testPrimitiveArrayTypeConverter() {
		Class<?> class1 = int[].class;
		Compound classTerm = jpc.toTerm(class1);
		Class<?> class2 = jpc.fromTerm(classTerm);
		assertEquals(class1, class2);
	}
	
	@Test
	public void testPrimitiveArrayTypeConverter2() {
		Class<?> class1 = int[][].class;
		Compound classTerm = jpc.toTerm(class1);
		Class<?> class2 = jpc.fromTerm(classTerm);
		assertEquals(class1, class2);
	}
	
	@Test
	public void testArrayTypeConverter() {
		Class<?> class1 = String[].class;
		Compound classTerm = jpc.toTerm(class1);
		Class<?> class2 = jpc.fromTerm(classTerm);
		assertEquals(class1, class2);
	}
	
	@Test
	public void testGenericArrayTypeConverter() {
		Type componentType = new TypeToken<List<String>>(){}.getType();
		Type type1 = genericArrayType(componentType);
		Compound typeTerm = jpc.toTerm(type1);
		Type type2 = jpc.fromTerm(typeTerm);
		assertEquals(type1, type2);
	}
	
	@Test
	public void testGenericArrayTypeConverter2() {
		Type componentType = new TypeToken<List<String>>(){}.getType();
		Type type1 = genericArrayType(componentType);
		type1 = genericArrayType(type1);
		Compound typeTerm = jpc.toTerm(type1);
		Type type2 = jpc.fromTerm(typeTerm);
		assertEquals(type1, type2);
	}
	
	
}
