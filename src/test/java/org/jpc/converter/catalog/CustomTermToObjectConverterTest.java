package org.jpc.converter.catalog;

import static java.util.Arrays.asList;
import static org.jpc.converter.catalog.CustomTermToObjectConverter.CUSTOM_TERM_FUNCTOR_NAME;
import static org.jpc.term.JRef.jRef;
import static org.junit.Assert.assertEquals;

import org.jpc.Jpc;
import org.jpc.JpcBuilder;
import org.jpc.converter.catalog.primitive.BooleanConverter;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.junit.Test;

public class CustomTermToObjectConverterTest {

	Jpc jpc = JpcBuilder.create().build();
	
	@Test
	public void testCustomConverterConversion() {
		Atom atom = new Atom("true");
		Term sourceTerm = new Compound(CUSTOM_TERM_FUNCTOR_NAME, asList(atom, jRef(new BooleanConverter())));
		assertEquals(true, jpc.fromTerm(sourceTerm));
	}
	
}
