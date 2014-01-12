package org.jpc.engine.embedded;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.jpc.engine.embedded.database.ArgumentIndexFunction;
import org.jpc.engine.embedded.database.IndexDescriptor;
import org.jpc.engine.embedded.database.UpdatableIndexFunction;
import org.jpc.query.Query;
import org.jpc.query.Solution;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Functor;
import org.jpc.term.JRef;
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
		IndexDescriptor indexDescriptor = new IndexDescriptor(new UpdatableIndexFunction(ArgumentIndexFunction.firstArgumentFunction()));
		engine.getIndexManager().setIndexDescriptor(new Functor(new Atom("a"), 3), indexDescriptor);
		queryingCompound(engine);
	}
	
	@Test
	public void testQueryJRef() {
		JpcEngine engine = new JpcEngine();
		Object o1 = new Object();
		Object o2 = new Object();
		Term jref = new JRef(o1);
		String cmpName = "jref";
		Compound compound1 = new Compound(cmpName, asList(jref));
		Compound compound2 = new Compound(cmpName, asList(new JRef(o1)));
		Compound compound3 = new Compound(cmpName, asList(new JRef(o2)));
		engine.assertz(compound1);
		assertTrue(engine.query(compound1).hasSolution());
		assertTrue(engine.query(compound2).hasSolution());
		assertFalse(engine.query(compound3).hasSolution());
		engine.assertz(compound3);
		String varName = "X";
		Query query = engine.query(new Compound(cmpName, asList(new Var("X"))));
		
		Solution solution = query.next();
		//assertEquals(o1, solution.getObject(varName));
		JRef jrefSolution = (JRef)solution.get(varName);
		assertEquals(o1, jrefSolution.getRef());
		
		solution = query.next();
		//assertEquals(o2, solution.getObject(varName));
		jrefSolution = (JRef) solution.get(varName);
		assertEquals(o2, jrefSolution.getRef());
		
		assertFalse(query.hasNext());
	}

}
