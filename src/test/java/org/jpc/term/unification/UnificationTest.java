package org.jpc.term.unification;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.Term;
import org.jpc.term.Var;
import org.jpc.util.JpcPreferences;
import org.junit.Test;

public class UnificationTest {

	@Test
	public void testVariableAnonymizer() {
		Term term = new Compound("x", asList(new Var("X"), Var.ANONYMOUS_VAR, new Var("Y"), new Var("X")));
		Term anonymizedTerm = term.termExpansion(new VariableAnonymizerTermExpander());
		Var var1 = new Var(JpcPreferences.JPC_VAR_PREFIX+0);
		Var var2 = new Var(JpcPreferences.JPC_VAR_PREFIX+1);
		assertTrue(anonymizedTerm.termEquals(new Compound("x", asList(var1, Var.ANONYMOUS_VAR, var2, var1))));
	}
	
	@Test
	public void testSuccessfulConstantUnification() {
		assertEquals(0, new IntegerTerm(0).unifyVars(new IntegerTerm(0), true).entrySet().size());
		
		Compound compound1 = new Compound("x", asList(new IntegerTerm(0), new Atom("hello")));
		Compound compound2 = new Compound("x", asList(new IntegerTerm(0), new Atom("hello")));
		assertEquals(0, compound1.unifyVars(compound2, true).entrySet().size());
	}
	
	@Test
	public void testFailedConstantUnification() {
		try {
			new IntegerTerm(0).unifyVars(new FloatTerm(0), true);
			fail();
		} catch(NonUnifiableException e) {}
		
		Compound compound1 = new Compound("x", asList(new IntegerTerm(0), new Atom("hello")));
		Compound compound2 = new Compound("y", asList(new IntegerTerm(0), new Atom("hello")));
		try {
			compound1.unifyVars(compound2, true);
			fail();
		} catch(NonUnifiableException e) {}
		
		compound2 = new Compound("x", asList(new IntegerTerm(1), new Atom("hello")));
		try {
			compound1.unifyVars(compound2, true);
			fail();
		} catch(NonUnifiableException e) {}
		
		compound2 = new Compound("x", asList(new IntegerTerm(0), new Atom("hell")));
		try {
			compound1.unifyVars(compound2, true);
			fail();
		} catch(NonUnifiableException e) {}
		
	}
	
	@Test
	public void testAnonymousVarUnification() {
		assertEquals(0, Var.ANONYMOUS_VAR.unifyVars(Var.ANONYMOUS_VAR, true).entrySet().size());
		
		Compound compound = new Compound("x", asList(new IntegerTerm(0), new Atom("hello")));
		assertEquals(0, Var.ANONYMOUS_VAR.unifyVars(new IntegerTerm(0), true).entrySet().size());
		assertEquals(0, Var.ANONYMOUS_VAR.unifyVars(new Atom("hello"), true).entrySet().size());
		assertEquals(0, Var.ANONYMOUS_VAR.unifyVars(compound, true).entrySet().size());
		
		assertEquals(0, new IntegerTerm(0).unifyVars(Var.ANONYMOUS_VAR, true).entrySet().size());
		assertEquals(0, new Atom("hello").unifyVars(Var.ANONYMOUS_VAR, true).entrySet().size());
		assertEquals(0, compound.unifyVars(Var.ANONYMOUS_VAR, true).entrySet().size());
	}

	@Test
	public void testSimpleUnification() {
		assertTrue(Var.ANONYMOUS_VAR.termEquals(Var.ANONYMOUS_VAR.unify(new Var("X"), true)));
		assertEquals(new Var("X"), new Var("X").unify(Var.ANONYMOUS_VAR, true));
		assertEquals(new IntegerTerm(0), new Var("X").unify(new IntegerTerm(0), true));
		assertEquals(new IntegerTerm(0), new IntegerTerm(0).unify(new Var("X"), true));
		assertEquals(new Var("X"), new Var("X").unify(new Var("Y"), true));
	}
	
	@Test
	public void testCompoundUnification1() {
		Compound compound1 = new Compound("x", asList(new IntegerTerm(0), new Var("X")));
		Compound compound2 = new Compound("x", asList(new Var("Y"), new Var("Y")));
		Compound expected = new Compound("x", asList(new IntegerTerm(0), new IntegerTerm(0)));
		assertEquals(expected, compound1.unify(compound2, true));
		
		compound1 = new Compound("x", asList(new IntegerTerm(0), new IntegerTerm(0)));
		assertEquals(expected, compound1.unify(compound2, true));
		
		assertEquals(expected, compound2.unify(compound1, true));
		
		compound1 = new Compound("x", asList(new IntegerTerm(0), new IntegerTerm(1)));
		try {
			compound1.unify(compound2, true);
			fail();
		} catch(NonUnifiableException e) {}
		
		compound1 = new Compound("x", asList(new Var("X"), new IntegerTerm(0)));
		assertEquals(expected, compound1.unify(compound2, true));
		
		compound1 = new Compound("x", asList(new Var("X1"), new Var("X2")));
		expected = new Compound("x", asList(new Var("X2"), new Var("X2")));
		assertEquals(expected, compound1.unify(compound2, true));
	}
	
	@Test
	public void testCompoundUnification2() {
		Compound compound1 = new Compound("x", asList(new Var("X1"), new Var("X2"), new IntegerTerm(3), new Var("X3"), new Var("X4"), new Var("X2")));
		Compound compound2 = new Compound("x", asList(new Var("Y"), new Var("Y"), new Var("Y"), new Var("Z"), new Var("Z"), new Var("Z")));
		Compound expected = new Compound("x", asList(new IntegerTerm(3), new IntegerTerm(3), new IntegerTerm(3), 
				new IntegerTerm(3), new IntegerTerm(3), new IntegerTerm(3)));
		assertEquals(expected, compound1.unify(compound2, true));
		
		compound1 = new Compound("x", asList(new IntegerTerm(3), new Var("X1"), new Var("X2"), new Var("X3"), new Var("X4"), new Var("X2")));
		assertEquals(expected, compound1.unify(compound2, true));
		
		compound1 = new Compound("x", asList(new IntegerTerm(3), new Var("X1"), new Var("X2"), new Var("X3"), new Var("X4"), new IntegerTerm(4)));
		expected = new Compound("x", asList(new IntegerTerm(3), new IntegerTerm(3), new IntegerTerm(3), 
				new IntegerTerm(4), new IntegerTerm(4), new IntegerTerm(4)));
		assertEquals(expected, compound1.unify(compound2, true));
	}
	
}
