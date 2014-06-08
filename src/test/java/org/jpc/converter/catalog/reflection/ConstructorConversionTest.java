package org.jpc.converter.catalog.reflection;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Collections;

import org.jpc.Jpc;
import org.jpc.JpcBuilder;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.Term;
import org.junit.Test;

public class ConstructorConversionTest {

	private static Jpc jpc = JpcBuilder.create().build();
	
	@Test
	public void testPrimitiveConversions() {
		Term intTerm = new Compound("int", asList(new IntegerTerm(0)));
		assertEquals(new Integer(0), jpc.fromTerm(intTerm));
		intTerm = new Compound("int", asList(new FloatTerm(0)));
		assertEquals(new Integer(0), jpc.fromTerm(intTerm));
		
		Term doubleTerm = new Compound("double", asList(new FloatTerm(0)));
		assertEquals(new Double(0), jpc.fromTerm(doubleTerm));
		intTerm = new Compound("int", asList(new FloatTerm(0)));
		assertEquals(new Integer(0), jpc.fromTerm(intTerm));
		
		Term floatTerm = new Compound("float", asList(new FloatTerm(0)));
		assertEquals(new Float(0), jpc.fromTerm(floatTerm));
		
		Term booleanTerm = new Compound("boolean", asList(new Atom("true")));
		assertTrue((Boolean)jpc.fromTerm(booleanTerm));
		booleanTerm = new Compound("boolean", asList(new Atom("false")));
		assertFalse((Boolean)jpc.fromTerm(booleanTerm));
		booleanTerm = new Compound("boolean", asList(new Atom("fail")));
		assertFalse((Boolean)jpc.fromTerm(booleanTerm));
	}
	
	@Test
	public void testStringConstructor() {
		String s = "s";
		Term term = new Compound(String.class.getName(), asList(new Atom(s)));
		assertEquals(s, jpc.fromTerm(term));
	}
	
	@Test
	public void testArrayListConstructor() {
		Term term = new Compound(ArrayList.class.getName(), asList(new Compound("int", asList(new IntegerTerm(0)))));
		assertEquals(Collections.emptyList(), jpc.fromTerm(term));
	}
}
