package org.jpc.mapping.converter.catalog;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.jpc.Jpc;
import org.jpc.JpcBuilder;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;
import org.junit.Test;

public class TypedTermConversionsTest {

	Jpc jpc = JpcBuilder.create().build();
	
	@Test
	public void testTypedTermConversionTest() {
		Term typeTerm; // = jpc.toTerm(boolean.class);
		typeTerm = new Compound("type", asList(Atom.NIL, ListTerm.listTerm(new Atom("boolean"))));
		Term sourceTerm = new Compound(TypedTermToObjectConverter.TYPED_TERM_FUNCTOR_NAME, asList(new Atom("true"), typeTerm));
		assertTrue((Boolean)jpc.fromTerm(sourceTerm));
		typeTerm = new Compound("type", asList(new Atom("boolean")));
		sourceTerm = new Compound(TypedTermToObjectConverter.TYPED_TERM_FUNCTOR_NAME, asList(new Atom("true"), typeTerm));
		assertTrue((Boolean)jpc.fromTerm(sourceTerm));
	}
	
}
