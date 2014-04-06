package org.jpc.converter.catalog.reification.type;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.jpc.Jpc;
import org.jpc.JpcBuilder;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.Term;
import org.junit.Test;

public class TypedTermConversionsTest {

	Jpc jpc = JpcBuilder.create().build();
	
	@Test
	public void testTypedTermConversionTest() {
		Term typeTerm = jpc.toTerm(boolean.class);
		Term sourceTerm = new Compound(TypedTermToObjectConverter.TYPED_TERM_FUNCTOR_NAME, asList(new Atom("true"), typeTerm));
		assertTrue((Boolean)jpc.fromTerm(sourceTerm));
	}
	
	@Test
	public void testPrimitiveClassShortNotation() {
		Term intTerm = new Compound("int", asList(new IntegerTerm(1)));
		assertEquals(new Integer(1), jpc.fromTerm(intTerm));
		intTerm = new Compound("int", asList(new FloatTerm(1.0)));
		assertEquals(new Integer(1), jpc.fromTerm(intTerm));
		Term booleanTerm = new Compound("boolean", asList(new Atom("true")));
		assertTrue((Boolean)jpc.fromTerm(booleanTerm));
		booleanTerm = new Compound("boolean", asList(new Atom("false")));
		assertFalse((Boolean)jpc.fromTerm(booleanTerm));
		booleanTerm = new Compound("boolean", asList(new Atom("fail")));
		assertFalse((Boolean)jpc.fromTerm(booleanTerm));
	}
	
}
