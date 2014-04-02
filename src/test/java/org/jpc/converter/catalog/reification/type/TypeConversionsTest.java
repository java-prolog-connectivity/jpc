package org.jpc.converter.catalog.reification.type;


import static org.junit.Assert.assertEquals;

import java.lang.reflect.Type;
import java.util.List;

import org.jpc.Jpc;
import org.jpc.JpcBuilder;
import org.jpc.converter.catalog.reification.type.TypeConversionsTest.A.B.C;
import org.jpc.term.Compound;
import org.junit.Test;
import org.minitoolbox.reflection.reification.GenericArrayTypeImpl;
import org.minitoolbox.reflection.reification.StaticClass;

import com.google.common.reflect.TypeToken;

public class TypeConversionsTest {

	Jpc jpc = JpcBuilder.create().build();
	
	public static class A {
		public static class B {
			public static class C {}
		}
	}
	
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
		Type type1 = new GenericArrayTypeImpl(componentType);
		Compound typeTerm = jpc.toTerm(type1);
		Type type2 = jpc.fromTerm(typeTerm);
		assertEquals(type1, type2);
	}
	
	@Test
	public void testGenericArrayTypeConverter2() {
		Type componentType = new TypeToken<List<String>>(){}.getType();
		Type type1 = new GenericArrayTypeImpl(componentType);
		type1 = new GenericArrayTypeImpl(type1);
		Compound typeTerm = jpc.toTerm(type1);
		Type type2 = jpc.fromTerm(typeTerm);
		assertEquals(type1, type2);
	}
	
}
