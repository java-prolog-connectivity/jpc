package org.jpc.term.expansion;

import static java.util.Arrays.asList;
import static org.jpc.JpcPreferences.SUBSTITUTION_OPERATOR;
import static org.jpc.JpcPreferences.TERM_CONVERSION_BY_MAPPING_SYMBOL;
import static org.jpc.JpcPreferences.TERM_CONVERSION_BY_REFERENCE_SYMBOL;
import static org.jpc.JpcPreferences.TERM_CONVERSION_BY_SERIALIZATION_SYMBOL;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.jpc.DefaultJpc;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.junit.Test;

public class ParameterizedSymbolExpanderTest {

	@Test
	public void testCorrectSymbols() {
		ParameterizedSymbolExpander.verifyOrThrow(TERM_CONVERSION_BY_MAPPING_SYMBOL + 1);
		ParameterizedSymbolExpander.verifyOrThrow(TERM_CONVERSION_BY_REFERENCE_SYMBOL + 1);
		ParameterizedSymbolExpander.verifyOrThrow(TERM_CONVERSION_BY_SERIALIZATION_SYMBOL + 1);
	}
	
	@Test
	public void testSymbolsWithoutPosition() {
		try {
			ParameterizedSymbolExpander.verifyOrThrow(TERM_CONVERSION_BY_MAPPING_SYMBOL);
			fail();
		} catch(Exception e){}
		try {
			ParameterizedSymbolExpander.verifyOrThrow(TERM_CONVERSION_BY_REFERENCE_SYMBOL);
			fail();
		} catch(Exception e){}
		try {
			ParameterizedSymbolExpander.verifyOrThrow(TERM_CONVERSION_BY_SERIALIZATION_SYMBOL);
			fail();
		} catch(Exception e){}
	}
	
	@Test
	public void testSymbolsWrongPosition() {
		try {
			ParameterizedSymbolExpander.verifyOrThrow(TERM_CONVERSION_BY_MAPPING_SYMBOL + 1.0);
			fail();
		} catch(Exception e){}

	}
	
	@Test
	public void testSymbolsWithoutSpecifier() {
		try {
			ParameterizedSymbolExpander.verifyOrThrow("" + 1);
			fail();
		} catch(Exception e){}
	}

	@Test
	public void testSymbolsWrongSpecifier() {
		try {
			ParameterizedSymbolExpander.verifyOrThrow("x" + 1);
			fail();
		} catch(Exception e){}
	}
	
	
	@Test
	public void testExpandingMappedObject() {
		Object o = "hello";
		Term term = new Compound(SUBSTITUTION_OPERATOR, asList(new Atom("m1")));
		Term expandedTerm = new ParameterizedSymbolExpander(asList(o)).expand(term).get();
		Object o2 = new DefaultJpc().fromTerm(expandedTerm);
		assertEquals(o, o2);
	}
	
	@Test
	public void testExpandingReferencedObject() {
		Object o = "hello";
		Term term = new Compound(SUBSTITUTION_OPERATOR, asList(new Atom("r1")));
		Term expandedTerm = new ParameterizedSymbolExpander(asList(o)).expand(term).get();
		Object o2 = new DefaultJpc().fromTerm(expandedTerm);
		assertTrue(o==o2);
	}
	
	@Test
	public void testExpandingSerializedObject() {
		Object o = "hello";
		Term term = new Compound(SUBSTITUTION_OPERATOR, asList(new Atom("s1")));
		Term expandedTerm = new ParameterizedSymbolExpander(asList(o)).expand(term).get();
		Object o2 = new DefaultJpc().fromTerm(expandedTerm);
		assertEquals(o, o2);
		assertFalse(o==o2);
	}

}
