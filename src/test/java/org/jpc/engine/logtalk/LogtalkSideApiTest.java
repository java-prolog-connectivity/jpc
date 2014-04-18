package org.jpc.engine.logtalk;

import static org.jpc.engine.prolog.PrologEngines.defaultPrologEngine;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.term.Atom;
import org.jpc.term.IntegerTerm;
import org.jpc.term.Term;
import org.junit.Test;

public class LogtalkSideApiTest {

	@Test
	public void testEval() {
		Term term = defaultPrologEngine().query("java::eval(abc::toUpperCase,term(V))").oneSolutionOrThrow().get("V");
		assertEquals(new Atom("ABC"), term);
	}
	
	@Test
	public void testJava1() {
		Term term = defaultPrologEngine().query("java(abc)::toUpperCase returns term(V)").oneSolutionOrThrow().get("V");
		assertEquals(new Atom("ABC"), term);
	}
	
	@Test
	public void testJava2() {
		Term term = defaultPrologEngine().query("java(abc, term(V))::toUpperCase").oneSolutionOrThrow().get("V");
		assertEquals(new Atom("ABC"), term);
		term = defaultPrologEngine().query("java([a:1,b:2], term(X))::invoke(get('a'))").oneSolutionOrThrow().get("X");
		assertEquals(new IntegerTerm(1), term);
		term = defaultPrologEngine().query("java([a:1,b:2], term(X))::get('a')").oneSolutionOrThrow().get("X");
		assertEquals(new IntegerTerm(1), term);
	}
	
	@Test
	public void testSymbioticObject() {
		Term term;
		term = defaultPrologEngine().query("[a:1,b:2]::get('a') returns term(X)").oneSolutionOrThrow().get("X");
		assertEquals(new IntegerTerm(1), term);
	}

	public static class Fixture {
		public static String x;
		public static String y;
	}
	
	@Test
	public void testEvalSetStaticField() {
		Fixture.x = null;
		Fixture.y = null;
		defaultPrologEngine().query("java::eval(class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture'])::([x,hello]))").oneSolutionOrThrow();
		defaultPrologEngine().query("java::eval(class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture'])::([y,bye]))").oneSolutionOrThrow();
		assertEquals("hello", Fixture.x);
		assertEquals("bye", Fixture.y);
		Atom result = (Atom) defaultPrologEngine().query("java::eval(class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture'])::[x], term(X))").oneSolutionOrThrow().get("X");
		assertEquals("hello", result.getName());
		result = (Atom) defaultPrologEngine().query("java::eval(class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture'])::[y], term(Y))").oneSolutionOrThrow().get("Y");
		assertEquals("bye", result.getName());
	}
	
	@Test
	public void testShortSetStaticField() {
		Fixture.x = null;
		Fixture.y = null;
		defaultPrologEngine().query("class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture'])::[x,hello]").oneSolutionOrThrow();
		defaultPrologEngine().query("class([org,jpc,engine,logtalk],['LogtalkSideApiTest','Fixture'])::[y,bye]").oneSolutionOrThrow();
		assertEquals("hello", Fixture.x);
		assertEquals("bye", Fixture.y);
	}
	
	@Test
	public void testListIndex() {
		//IntegerTerm result = (IntegerTerm) defaultPrologEngine().query("java::eval([1,2,3]::[1], term(X))").oneSolutionOrThrow().get("X");
		IntegerTerm result = (IntegerTerm) defaultPrologEngine().query("[1,2,3]::[1] returns term(X)").oneSolutionOrThrow().get("X");
		assertEquals(2, result.intValue());
	}
	
	@Test
	public void testNew() {
		Term term = defaultPrologEngine().query("java(class([java,lang],['String']), term(V))::new").oneSolutionOrThrow().get("V");
		assertEquals(new Atom(""), term);
		term = defaultPrologEngine().query("java(class([java,lang],['String']))::new('hello') returns term(V)").oneSolutionOrThrow().get("V");
		assertEquals(new Atom("hello"), term);
	}
	
	@Test
	public void testCurrentEngine() {
		PrologEngine prologEngine = defaultPrologEngine().query("prolog_engines::this_engine(E)").<PrologEngine>selectObject("E").oneSolutionOrThrow();
		assertNotNull(prologEngine);
	}
	
}
