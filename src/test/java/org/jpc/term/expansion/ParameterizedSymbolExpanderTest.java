package org.jpc.term.expansion;

import static java.util.Arrays.asList;
import static org.jpc.JpcPreferences.SUBSTITUTION_OPERATOR;
import static org.jpc.JpcPreferences.TERM_CONVERSION_BY_MAPPING_SYMBOL;
import static org.jpc.JpcPreferences.TERM_CONVERSION_BY_REFERENCE_SYMBOL;
import static org.jpc.JpcPreferences.TERM_CONVERSION_BY_SERIALIZATION_SYMBOL;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.jpc.DefaultJpc;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.IntegerTerm;
import org.jpc.term.Term;
import org.junit.Test;

public class ParameterizedSymbolExpanderTest {

	@Test
	public void testCorrectSymbols() {
		ParameterizedSymbolExpander.verifyOrThrow(TERM_CONVERSION_BY_MAPPING_SYMBOL);
		ParameterizedSymbolExpander.verifyOrThrow(TERM_CONVERSION_BY_REFERENCE_SYMBOL);
		ParameterizedSymbolExpander.verifyOrThrow(TERM_CONVERSION_BY_SERIALIZATION_SYMBOL);
	}
	
	@Test
	public void testEmptySymbol() {
		try {
			ParameterizedSymbolExpander.verifyOrThrow("");
		} catch(Exception e){}
	}
	
	@Test
	public void testExpandingMappedObject() {
		Object o = "hello";
		Term term = new Compound(SUBSTITUTION_OPERATOR, asList(new Atom(TERM_CONVERSION_BY_MAPPING_SYMBOL), new IntegerTerm(1)));
		Term expandedTerm = new PositionalSymbolExpander(asList(o)).apply(term);
		Object o2 = new DefaultJpc().fromTerm(expandedTerm);
		assertEquals(o, o2);
	}
	
	@Test
	public void testExpandingReferencedObject() {
		Object o = "hello";
		Term term = new Compound(SUBSTITUTION_OPERATOR, asList(new Atom(TERM_CONVERSION_BY_REFERENCE_SYMBOL), new IntegerTerm(1)));
		Term expandedTerm = new PositionalSymbolExpander(asList(o)).apply(term);
		Object o2 = new DefaultJpc().fromTerm(expandedTerm);
		assertTrue(o==o2);
	}
	
	@Test
	public void testExpandingSerializedObject() {
		Object o = "hello";
		Term term = new Compound(SUBSTITUTION_OPERATOR, asList(new Atom(TERM_CONVERSION_BY_SERIALIZATION_SYMBOL), new IntegerTerm(1)));
		Term expandedTerm = new PositionalSymbolExpander(asList(o)).apply(term);
		Object o2 = new DefaultJpc().fromTerm(expandedTerm);
		assertEquals(o, o2);
		assertFalse(o==o2);
	}

}
