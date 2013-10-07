package org.jpc.term.expansion;

import static java.util.Arrays.asList;
import static org.jpc.JpcPreferences.CONVERSION_SPECIFIER_OPERATOR;
import static org.jpc.JpcPreferences.SUBSTITUTION_OPERATOR;
import static org.jpc.JpcPreferences.TERM_CONVERSION_BY_MAPPING_AND_REFERENCE_SYMBOL;
import static org.jpc.JpcPreferences.TERM_CONVERSION_BY_MAPPING_SYMBOL;
import static org.jpc.JpcPreferences.TERM_CONVERSION_BY_REFERENCE_SYMBOL;
import static org.jpc.JpcPreferences.TERM_CONVERSION_BY_SERIALIZATION_SYMBOL;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.jpc.DefaultJpc;
import org.jpc.Jpc;
import org.jpc.JpcBuilder;
import org.jpc.converter.JpcConverter;
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
		ParameterizedSymbolExpander.verifyOrThrow(TERM_CONVERSION_BY_MAPPING_AND_REFERENCE_SYMBOL);
		ParameterizedSymbolExpander.verifyOrThrow(TERM_CONVERSION_BY_SERIALIZATION_SYMBOL);
	}
	
	@Test
	public void testEmptySymbol() {
		try {
			ParameterizedSymbolExpander.verifyOrThrow("");
		} catch(Exception e){}
	}
	
	@Test
	public void testWrongSymbol() {
		try {
			ParameterizedSymbolExpander.verifyOrThrow("x");
		} catch(Exception e){}
	}
	
	@Test
	public void testExpandingMappedObject() {
		Object o = "hello";
		Term term = new Compound(CONVERSION_SPECIFIER_OPERATOR, asList(new Compound(SUBSTITUTION_OPERATOR, asList(new IntegerTerm(1))), new Atom(TERM_CONVERSION_BY_MAPPING_SYMBOL)));
		Term expandedTerm = new PositionalSymbolExpander(asList(o)).apply(term);
		Object o2 = new DefaultJpc().fromTerm(expandedTerm);
		assertEquals(o, o2);
	}
	
	@Test
	public void testExpandingMappedReferencedObject() {
		final Compound toTerm = new Compound("x", asList(new Atom("x")));
		class X {}
		JpcConverter<X, Compound> conv = new JpcConverter<X, Compound>() {
			@Override
			public Compound toTerm(X o, Jpc context) {
				return toTerm;
			}
		};
		Jpc context = JpcBuilder.create().registerConverter(conv).build();
		X o = new X();
		Term term = new Compound(CONVERSION_SPECIFIER_OPERATOR, asList(new Compound(SUBSTITUTION_OPERATOR, asList(new IntegerTerm(1))), new Atom(TERM_CONVERSION_BY_MAPPING_AND_REFERENCE_SYMBOL)));
		Term expandedTerm = new PositionalSymbolExpander(asList(o), context).apply(term);
		assertEquals(toTerm, expandedTerm);
		
		Object o2 = context.fromTerm(expandedTerm);
		assertTrue(o == o2);
	}
	
	@Test
	public void testExpandingReferencedObject() {
		Object o = "hello";
		Term term = new Compound(CONVERSION_SPECIFIER_OPERATOR, asList(new Compound(SUBSTITUTION_OPERATOR, asList(new IntegerTerm(1))), new Atom(TERM_CONVERSION_BY_REFERENCE_SYMBOL)));
		Term expandedTerm = new PositionalSymbolExpander(asList(o)).apply(term);
		Object o2 = new DefaultJpc().fromTerm(expandedTerm);
		assertTrue(o==o2);
	}
	
	@Test
	public void testExpandingSerializedObject() {
		Object o = "hello";
		Term term = new Compound(CONVERSION_SPECIFIER_OPERATOR, asList(new Compound(SUBSTITUTION_OPERATOR, asList(new IntegerTerm(1))), new Atom(TERM_CONVERSION_BY_SERIALIZATION_SYMBOL)));
		Term expandedTerm = new PositionalSymbolExpander(asList(o)).apply(term);
		Object o2 = new DefaultJpc().fromTerm(expandedTerm);
		assertEquals(o, o2);
		assertFalse(o==o2);
	}

}
