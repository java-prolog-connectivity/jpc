package org.jpc.engine.embedded;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.jpc.engine.embedded.indexing.ArgumentIndexFunction;
import org.jpc.query.Query;
import org.jpc.query.Solution;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Functor;
import org.jpc.term.Term;
import org.jpc.term.Var;
import org.junit.Test;

public class JpcQueryTest {

	private void assertQueryAndRetract(JpcEngine engine, Term term) {
		assertFalse(engine.query(term).hasSolution());
		assertEquals(0, engine.query(term).allSolutions().size());
		engine.assertz(term);
		assertTrue(engine.query(term).hasSolution());
		assertEquals(1, engine.query(term).allSolutions().size());
		engine.assertz(term);
		assertTrue(engine.query(term).hasSolution());
		assertEquals(2, engine.query(term).allSolutions().size());
		engine.retractOne(term);
		assertTrue(engine.query(term).hasSolution());
		assertEquals(1, engine.query(term).allSolutions().size());
		engine.retractOne(term);
		assertFalse(engine.query(term).hasSolution());
		assertEquals(0, engine.query(term).allSolutions().size());
		
		engine.assertz(term);
		engine.assertz(term);
		assertEquals(2, engine.query(term).allSolutions().size());
		engine.retractAll(term);
		assertFalse(engine.query(term).hasSolution());
		assertEquals(0, engine.query(term).allSolutions().size());
	}
	
	
	@Test
	public void testAssertAtom() {
		assertQueryAndRetract(new JpcEngine(), new Atom("x"));
	}
	
	@Test
	public void testAssertGroundCompound() {
		Compound compound = new Compound("x", asList(new Atom("x"), new Atom("y"), new Atom("z")));
		assertQueryAndRetract(new JpcEngine(), compound);
	}
	
	@Test
	public void testAssertNonGroundCompound() {
		Compound compound = new Compound("x", asList(new Var("X"), new Atom("y"), new Atom("z")));
		assertQueryAndRetract(new JpcEngine(), compound);
		compound = new Compound("x", asList(new Var("X"), new Var("X"), new Var("X")));
		assertQueryAndRetract(new JpcEngine(), compound);
		compound = new Compound("x", asList(Var.ANONYMOUS_VAR, Var.ANONYMOUS_VAR, Var.ANONYMOUS_VAR));
		assertQueryAndRetract(new JpcEngine(), compound);
	}
	
	private void queryingCompound(JpcEngine engine) {
		Term term = new Compound("a", asList(new Atom("x"), new Atom("y"), new Var("Z")));
		engine.assertz(term);
		Term goal = new Compound("a", asList(new Atom("x"), new Var("Y"), new Atom("z")));
		Query query = engine.query(goal);
		Solution aSolution = query.oneSolution().get();
		assertEquals(new Atom("y"), aSolution.get("Y"));
		
		term = new Compound("a", asList(new Atom("x"), new Atom("y"), new Atom("z")));
		engine.assertz(term);
		
		term = new Compound("a", asList(new Atom("x"), new Var("Y"), new Atom("z")));
		engine.assertz(term);
		
		query = engine.query(goal);
		assertEquals(3, query.allSolutions().size());
	}
	
	@Test
	public void testQueryCompound() {
		queryingCompound(new JpcEngine());
	}
	
	@Test
	public void testQueryIndexedCompound() {
		JpcEngine engine = new JpcEngine();
		engine.getIndexManager().setIndex(new Functor(new Atom("a"), 3), ArgumentIndexFunction.firstArgumentFunction());
		queryingCompound(engine);
	}
}
