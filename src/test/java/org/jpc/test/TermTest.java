package org.jpc.test;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.term.Variable;
import org.junit.Test;

public class TermTest {
	Term t1 = new Compound("yellow", asList(new Compound("blue", asList(new Atom("red")))));
	
	@Test
	public void testEquality() {
		Term t2 = new Compound("yellow", asList(new Compound("blue", asList(new Atom("red")))));
		assertTrue(t1.equals(t2));
	}
	
	@Test
	public void testStructure() {
		assertTrue(t1 instanceof Compound);
		assertEquals(t1.arity(), 1);
		assertEquals(t1.arg(1).arity(), 1);
		assertEquals(t1.arg(1).arg(1).arity(), 0);
	}
	
	@Test
	public void testVariables() {
		new Variable("VarName"); //fine
		new Variable("_varName"); //fine
		new Variable("__VarName"); //fine
		new Variable("_"); //fine
		try {
			new Variable("varName"); //wrong, variables should start with capital letters
			fail();
		} catch(Exception e){}//expected
		try {
			new Variable("1varName"); //wrong, variables should start with capital letters
			fail();
		} catch(Exception e){}//expected
	}

}
