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
import org.jpc.term.compiled.CompiledVar;
import org.junit.Test;

public class UnificationTest {
	
	
	private void allUnifications(Term t1, Term t2, Term expected) {
		Term unification = t1.unify(t2);
		assertTrue(expected.termEquals(unification));
		
		Term t1Compiled = t1.compile(0);
		Term t2Compiled = t2.compile(1);
		unification = t1Compiled.unify(t2Compiled);
		assertTrue(expected.termEquals(unification));
		
		Term t1CompiledForQuery = t1.compileForQuery();
		unification = t1CompiledForQuery.unify(t2Compiled);
		assertTrue(expected.termEquals(unification));
	}
	
	
	private void allFailedUnifications(Term t1, Term t2) {
		try {
			t1.unify(t2);
			fail();
		} catch(NonUnifiableException e) {}
		
		Term t1Compiled = t1.compile(0);
		Term t2Compiled = t2.compile(0);
		try {
			t1Compiled.unify(t2Compiled);
			fail();
		} catch(NonUnifiableException e) {}
		
		Term t1CompiledForQuery = t1.compileForQuery();
		try {
			t1CompiledForQuery.unify(t2Compiled);
			fail();
		} catch(NonUnifiableException e) {}
		
		Term t2CompiledForQuery = t2.compileForQuery();
		try {
			t2CompiledForQuery.unify(t1Compiled);
			fail();
		} catch(NonUnifiableException e) {}
	}
	
	
	@Test
	public void testSuccessfulConstantUnification() {
		IntegerTerm iterm1 = new IntegerTerm(0);
		IntegerTerm iterm2 = new IntegerTerm(0);
		assertEquals(0, iterm1.unifyVars(iterm2).entrySet().size());
		allUnifications(iterm1, iterm2, iterm1);
		
		Compound compound1 = new Compound("x", asList(new IntegerTerm(0), new Atom("hello")));
		Compound compound2 = new Compound("x", asList(new IntegerTerm(0), new Atom("hello")));
		assertEquals(0, compound1.unifyVars(compound2).entrySet().size());
		allUnifications(compound1, compound2, compound1);
	}
	
	@Test
	public void testFailedConstantUnification() {
		allFailedUnifications(new IntegerTerm(0), new FloatTerm(0));
		
		Compound compound1 = new Compound("x", asList(new IntegerTerm(0), new Atom("hello")));
		Compound compound2 = new Compound("y", asList(new IntegerTerm(0), new Atom("hello")));
		allFailedUnifications(compound1, compound2);

		compound2 = new Compound("x", asList(new IntegerTerm(1), new Atom("hello")));
		allFailedUnifications(compound1, compound2);
		
		compound2 = new Compound("x", asList(new IntegerTerm(0), new Atom("hell")));
		allFailedUnifications(compound1, compound2);
	}
	
	@Test
	public void testAnonymousVarUnification() {
		assertEquals(0, Var.ANONYMOUS_VAR.unifyVars(Var.ANONYMOUS_VAR).entrySet().size());
		allUnifications(Var.ANONYMOUS_VAR, Var.ANONYMOUS_VAR, Var.ANONYMOUS_VAR);
		
		assertEquals(0, Var.ANONYMOUS_VAR.unifyVars(new IntegerTerm(0)).entrySet().size());
		allUnifications(Var.ANONYMOUS_VAR, new IntegerTerm(0), Var.ANONYMOUS_VAR);
		
		assertEquals(0, Var.ANONYMOUS_VAR.unifyVars(new Atom("hello")).entrySet().size());
		allUnifications(Var.ANONYMOUS_VAR, new Atom("hello"), Var.ANONYMOUS_VAR);
		
		Compound compound = new Compound("x", asList(new IntegerTerm(0), new Atom("hello")));
		assertEquals(0, Var.ANONYMOUS_VAR.unifyVars(compound).entrySet().size());
		allUnifications(Var.ANONYMOUS_VAR, compound, Var.ANONYMOUS_VAR);
		
		assertEquals(0, new IntegerTerm(0).unifyVars(Var.ANONYMOUS_VAR).entrySet().size());
		allUnifications(new IntegerTerm(0), Var.ANONYMOUS_VAR, new IntegerTerm(0));
		
		assertEquals(0, new Atom("hello").unifyVars(Var.ANONYMOUS_VAR).entrySet().size());
		allUnifications(new Atom("hello"), Var.ANONYMOUS_VAR, new Atom("hello"));
		
		assertEquals(0, compound.unifyVars(Var.ANONYMOUS_VAR).entrySet().size());
		allUnifications(compound, Var.ANONYMOUS_VAR, compound);
	}

	@Test
	public void testSimpleUnification() {
		assertTrue(Var.ANONYMOUS_VAR.termEquals(Var.ANONYMOUS_VAR.unify(new Var("X"))));
		CompiledVar anonymousCompiledVar = CompiledVar.anonymousVar(0);
		assertTrue(Var.ANONYMOUS_VAR.termEquals(anonymousCompiledVar.unify(new Var("X"))));
		assertEquals(new Var("X"), new Var("X").unify(Var.ANONYMOUS_VAR));
		assertEquals(new IntegerTerm(0), new Var("X").unify(new IntegerTerm(0)));
		assertEquals(new IntegerTerm(0), new IntegerTerm(0).unify(new Var("X")));
		assertEquals(new Var("X"), new Var("X").unify(new Var("Y")));
	}
	
	@Test
	public void testCompoundUnification1() {
		Compound compound1 = new Compound("x", asList(new IntegerTerm(0), new Var("X")));
		Compound compound2 = new Compound("x", asList(new Var("Y"), new Var("Y")));
		Compound expected = new Compound("x", asList(new IntegerTerm(0), new IntegerTerm(0)));
		allUnifications(compound1, compound2, expected);
		
		compound1 = new Compound("x", asList(new IntegerTerm(0), new IntegerTerm(0)));
		allUnifications(compound1, compound2, expected);
		allUnifications(compound2, compound1, expected);
		
		compound1 = new Compound("x", asList(new IntegerTerm(0), new IntegerTerm(1)));
		allFailedUnifications(compound1, compound2);
		
		compound1 = new Compound("x", asList(new Var("X"), new IntegerTerm(0)));
		allUnifications(compound1, compound2, expected);
		
		compound1 = new Compound("x", asList(new Var("X1"), new Var("X2")));
		expected = new Compound("x", asList(new Var("X2"), new Var("X2"))); //testing the expected variable names.
		assertEquals(expected, compound1.unify(compound2));
	}
	
	@Test
	public void testCompoundUnification2() {
		Compound compound1 = new Compound("x", asList(new Var("X1"), new Var("X2"), new IntegerTerm(3), new Var("X3"), new Var("X4"), new Var("X2")));
		Compound compound2 = new Compound("x", asList(new Var("Y"), new Var("Y"), new Var("Y"), new Var("Z"), new Var("Z"), new Var("Z")));
		Compound expected = new Compound("x", asList(new IntegerTerm(3), new IntegerTerm(3), new IntegerTerm(3), 
				new IntegerTerm(3), new IntegerTerm(3), new IntegerTerm(3)));
		allUnifications(compound1, compound2, expected);
		
		compound1 = new Compound("x", asList(new IntegerTerm(3), new Var("X1"), new Var("X2"), new Var("X3"), new Var("X4"), new Var("X2")));
		allUnifications(compound1, compound2, expected);
		
		compound1 = new Compound("x", asList(new IntegerTerm(3), new Var("X1"), new Var("X2"), new Var("X3"), new Var("X4"), new IntegerTerm(4)));
		expected = new Compound("x", asList(new IntegerTerm(3), new IntegerTerm(3), new IntegerTerm(3), 
				new IntegerTerm(4), new IntegerTerm(4), new IntegerTerm(4)));
		allUnifications(compound1, compound2, expected);
	}
	
}