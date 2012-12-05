package org.jpc.salt;

import static java.util.Arrays.asList;
import static org.jpc.term.Variable.ANONYMOUS_VAR;
import static org.junit.Assert.assertTrue;

import org.jpc.salt.JpcTermWriter;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;
import org.jpc.term.Variable;
import org.junit.Test;

public class JpcTermWriterTest {

	Term aAtom = new Atom("A");
	Term aVar = new Variable("A");
	Term bVar = new Variable("B");
	Term namedAnonVar = new Variable("_A");
	
	Term t3 = new Compound(aAtom, asList(new Compound(ANONYMOUS_VAR, asList(
			new ListTerm(asList(aAtom, aVar, bVar, namedAnonVar, ANONYMOUS_VAR))))));
	
	@Test
	public void testTermWriter() {
		JpcTermWriter termWriter = new JpcTermWriter();
		t3.read(termWriter);
		assertTrue(t3.termEquals(termWriter.getTerms().get(0)));
	}
}
