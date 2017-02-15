package org.jpc.util.salt;

import static java.util.Arrays.asList;
import static org.jpc.term.ListTerm.listTerm;
import static org.jpc.term.Var.dontCare;
import static org.jpc.util.termprocessor.JpcTermCollector.termCollector;
import static org.junit.Assert.assertTrue;

import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.term.Var;
import org.jpc.util.termprocessor.JpcTermCollector;
import org.junit.Test;

public class JpcTermWriterTest {

	Term aAtom = new Atom("A");
	Term aVar = new Var("A");
	Term bVar = new Var("B");
	Term namedAnonVar = new Var("_A");
	
	Term t3 = new Compound(aAtom, asList(new Compound(dontCare(), asList(
			listTerm(asList(aAtom, aVar, bVar, namedAnonVar, dontCare()))))));
	
	@Test
	public void testTermWriter() {
		JpcTermCollector collector = termCollector();
		JpcTermStreamer termWriter = new JpcTermStreamer(collector);
		t3.read(termWriter);
		assertTrue(t3.termEquals(collector.getFirst()));
	}
}
