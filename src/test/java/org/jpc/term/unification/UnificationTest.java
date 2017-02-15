package org.jpc.term.unification;

import static java.util.Arrays.asList;
import static org.jpc.term.Var.dontCare;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Float;
import org.jpc.term.Integer;
import org.jpc.term.Term;
import org.jpc.term.Var;
import org.jpc.term.compiler.Environment;
import org.junit.Test;

public class UnificationTest {
	
	private void allUnifications(Term t1, Term t2, Term expected) {
		Term unification = t1.compileAndUnify(t2);
		assertTrue(expected.termEquals(unification));
		
		Term t1Compiled = t1.compile();
		Term t2Compiled = t2.compile();
		unification = t1Compiled.unifyAndBind(t2Compiled);
		assertTrue(expected.termEquals(unification));
		
		Term t2CompiledForQuery = t2.compile(true);
		unification = t1Compiled.unifyAndBind(t2CompiledForQuery);
		assertTrue(expected.termEquals(unification));
	}
	
	
	private void allFailedUnifications(Term t1, Term t2) {
		try {
			t1.compileAndUnify(t2);
			fail();
		} catch(NonUnifiableException e) {}
		
		Environment env = new Environment();
		Term t1Compiled = t1.compile(env);
		Term t2Compiled = t2.compile(env);
		try {
			t1Compiled.unifyAndBind(t2Compiled);
			fail();
		} catch(NonUnifiableException e) {}
		
		Term t1CompiledForQuery = t1.compile(true);
		try {
			t1CompiledForQuery.unifyAndBind(t2Compiled);
			fail();
		} catch(NonUnifiableException e) {}
		
		Term t2CompiledForQuery = t2.compile(true);
		try {
			t1Compiled.unifyAndBind(t2CompiledForQuery);
			fail();
		} catch(NonUnifiableException e) {}

	}
	
	
	@Test
	public void testSuccessfulConstantUnification() {
		Integer iterm1 = new Integer(0);
		Integer iterm2 = new Integer(0);
		assertEquals(0, iterm1.compileAndUnifyVars(iterm2).entrySet().size());
		allUnifications(iterm1, iterm2, iterm1);
		
		Compound compound1 = new Compound("x", asList(new Integer(0), new Atom("hello")));
		Compound compound2 = new Compound("x", asList(new Integer(0), new Atom("hello")));
		assertEquals(0, compound1.compileAndUnifyVars(compound2).entrySet().size());
		allUnifications(compound1, compound2, compound1);
	}
	
	@Test
	public void testFailedConstantUnification() {
		allFailedUnifications(new Integer(0), new Float(0));
		
		Compound compound1 = new Compound("x", asList(new Integer(0), new Atom("hello")));
		Compound compound2 = new Compound("y", asList(new Integer(0), new Atom("hello")));
		allFailedUnifications(compound1, compound2);

		compound2 = new Compound("x", asList(new Integer(1), new Atom("hello")));
		allFailedUnifications(compound1, compound2);
		
		compound2 = new Compound("x", asList(new Integer(0), new Atom("hell")));
		allFailedUnifications(compound1, compound2);
	}
	
	@Test
	public void testAnonymousVarUnification() {
		assertEquals(0, dontCare().compileAndUnifyVars(dontCare()).entrySet().size());
		allUnifications(dontCare(), dontCare(), dontCare());
		
		assertEquals(0, dontCare().compileAndUnifyVars(new Integer(0)).entrySet().size());
		allUnifications(dontCare(), new Integer(0), dontCare());
		
		assertEquals(0, dontCare().compileAndUnifyVars(new Atom("hello")).entrySet().size());
		allUnifications(dontCare(), new Atom("hello"), dontCare());
		
		Compound compound = new Compound("x", asList(new Integer(0), new Atom("hello")));
		assertEquals(0, dontCare().compileAndUnifyVars(compound).entrySet().size());
		allUnifications(dontCare(), compound, dontCare());
		
		assertEquals(0, new Integer(0).compileAndUnifyVars(dontCare()).entrySet().size());
		allUnifications(new Integer(0), dontCare(), new Integer(0));
		
		assertEquals(0, new Atom("hello").compileAndUnifyVars(dontCare()).entrySet().size());
		allUnifications(new Atom("hello"), dontCare(), new Atom("hello"));
		
		assertEquals(0, compound.compileAndUnifyVars(dontCare()).entrySet().size());
		allUnifications(compound, dontCare(), compound);
	}

	@Test
	public void testSimpleUnification() {
		assertTrue(dontCare().termEquals(dontCare().compileAndUnify(new Var("X"))));
		assertEquals(new Var("X"), new Var("X").compileAndUnify(dontCare()));
		assertEquals(new Integer(0), new Var("X").compileAndUnify(new Integer(0)));
		assertEquals(new Integer(0), new Integer(0).compileAndUnify(new Var("X")));
		assertEquals(new Var("X"), new Var("X").compileAndUnify(new Var("Y")));
	}
	
	@Test
	public void testCompoundUnification1() {
		Compound compound1 = new Compound("x", asList(new Compound("x", asList(new Var("X"))), new Var("X")));
		Compound compound2 = new Compound("x", asList(new Var("Y"), new Integer(0)));
		Compound expected = new Compound("x", asList(new Compound("x", asList(new Integer(0))), new Integer(0)));
		allUnifications(compound1, compound2, expected);
		allUnifications(compound2, compound1, expected);
		
		compound2 = new Compound("x", asList(dontCare(), new Integer(0)));
		allUnifications(compound1, compound2, expected);
	}
	
	@Test
	public void testCompoundUnification2() {
		Compound compound1 = new Compound("x", asList(new Integer(0), new Var("X")));
		Compound compound2 = new Compound("x", asList(new Var("Y"), new Var("Y")));
		Compound expected = new Compound("x", asList(new Integer(0), new Integer(0)));
		allUnifications(compound1, compound2, expected);
		
		compound1 = new Compound("x", asList(new Integer(0), new Integer(0)));
		allUnifications(compound1, compound2, expected);
		allUnifications(compound2, compound1, expected);
		
		compound1 = new Compound("x", asList(new Integer(0), new Integer(1)));
		allFailedUnifications(compound1, compound2);
		
		compound1 = new Compound("x", asList(new Var("X"), new Integer(0)));
		allUnifications(compound1, compound2, expected);
		
		compound1 = new Compound("x", asList(new Var("X1"), new Var("X2")));
		expected = new Compound("x", asList(new Var("X2"), new Var("X2"))); //testing the expected variable names.
		assertEquals(expected, compound1.compileAndUnify(compound2));
	}
	
	@Test
	public void testCompoundUnification3() {
		Compound compound1 = new Compound("x", asList(new Var("X1"), new Var("X2"), new Integer(3), new Var("X3"), new Var("X4"), new Var("X2")));
		Compound compound2 = new Compound("x", asList(new Var("Y"), new Var("Y"), new Var("Y"), new Var("Z"), new Var("Z"), new Var("Z")));
		Compound expected = new Compound("x", asList(new Integer(3), new Integer(3), new Integer(3),
				new Integer(3), new Integer(3), new Integer(3)));
		allUnifications(compound1, compound2, expected);
		
		compound1 = new Compound("x", asList(new Integer(3), new Var("X1"), new Var("X2"), new Var("X3"), new Var("X4"), new Var("X2")));
		allUnifications(compound1, compound2, expected);
		
		compound1 = new Compound("x", asList(new Integer(3), new Var("X1"), new Var("X2"), new Var("X3"), new Var("X4"), new Integer(4)));
		expected = new Compound("x", asList(new Integer(3), new Integer(3), new Integer(3),
				new Integer(4), new Integer(4), new Integer(4)));
		allUnifications(compound1, compound2, expected);
	}
	
}
