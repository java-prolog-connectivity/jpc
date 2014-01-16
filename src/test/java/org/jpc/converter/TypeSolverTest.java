package org.jpc.converter;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Vector;

import org.jgum.JGum;
import org.jpc.converter.typesolver.JGumTypeSolverManager;
import org.jpc.converter.typesolver.TypeSolver;
import org.jpc.converter.typesolver.TypeSolverManager;
import org.jpc.converter.typesolver.UnrecognizedObjectException;
import org.junit.Test;

public class TypeSolverTest {

	class ListClass {}
	
	class UnrecognizedObjectExceptionTypeSolver implements TypeSolver<List> {
		@Override
		public Type getType(List object) {
			throw new UnrecognizedObjectException();
		}
	}
	
	class NonGenericTypeSolver implements TypeSolver {
		@Override
		public Type getType(Object object) {
			return ListClass.class;
		}
	}
	
	class GenericListTypeSolver implements TypeSolver<List> {
		@Override
		public Type getType(List object) {
			return ListClass.class;
		}
	}
	
	class FullGenericListTypeSolver implements TypeSolver<List<List<String>>> {
		@Override
		public Type getType(List object) {
			return ListClass.class;
		}
	}
	
	class SingleBoundListTypeSolver<T extends List> implements TypeSolver<T> {
		@Override
		public Type getType(T object) {
			return ListClass.class;
		}
	}
	
	class MultipleBoundsListTypeSolver<T extends Iterable & List> implements TypeSolver<T> {
		@Override
		public Type getType(T object) {
			return ListClass.class;
		}
	}
	
	@Test
	public void testNonGenericListTypeSolver() {
		TypeSolverManager manager = new JGumTypeSolverManager(new JGum());
		Object key = new Object();
		manager.register(key, new NonGenericTypeSolver());
		assertEquals(ListClass.class, manager.getType(key, new Object()));
		assertEquals(ListClass.class, manager.getType(key, new ArrayList()));
	}
	
	@Test
	public void testUnrecognizedObjectExceptionTypeSolver() {
		TypeSolverManager manager = new JGumTypeSolverManager(new JGum());
		Object key = new Object();
		manager.register(key, new NonGenericTypeSolver());
		manager.register(key, new UnrecognizedObjectExceptionTypeSolver());
		assertEquals(ListClass.class, manager.getType(key, new ArrayList()));
	}
	
	@Test
	public void testUnrecognizedObjectExceptionTypeSolver2() {
		TypeSolverManager manager = new JGumTypeSolverManager(new JGum());
		Object key = new Object();
		manager.register(key, new UnrecognizedObjectExceptionTypeSolver());
		try {
			manager.getType(new Object(), new Object());
			fail();
		} catch(UnrecognizedObjectException e) {}
		manager.register(key, new NonGenericTypeSolver());
		assertEquals(ListClass.class, manager.getType(key, new ArrayList()));
	}
	
	@Test
	public void testNoTypeSolver() {
		TypeSolverManager manager = new JGumTypeSolverManager(new JGum());
		try {
			manager.getType(new Object(), new Object());
			fail();
		} catch(UnrecognizedObjectException e) {}
	}

	@Test
	public void testGenericListTypeSolver() {
		TypeSolverManager manager = new JGumTypeSolverManager(new JGum());
		Object key = new Object();
		manager.register(key, new GenericListTypeSolver());
		try {
			manager.getType(key, new Object());
			fail();
		} catch(UnrecognizedObjectException e) {}
		assertEquals(ListClass.class, manager.getType(key, new ArrayList()));
	}
	
	@Test
	public void testFullGenericListTypeSolver() {
		TypeSolverManager manager = new JGumTypeSolverManager(new JGum());
		Object key = new Object();
		manager.register(key, new FullGenericListTypeSolver());
		try {
			manager.getType(key, new Object());
			fail();
		} catch(UnrecognizedObjectException e) {}
		assertEquals(ListClass.class, manager.getType(key, new ArrayList()));
	}
	
	@Test
	public void testSingleBoundListTypeSolver() {
		TypeSolverManager manager = new JGumTypeSolverManager(new JGum());
		Object key = new Object();
		manager.register(key, new SingleBoundListTypeSolver());
		try {
			manager.getType(key, new Object());
			fail();
		} catch(UnrecognizedObjectException e) {}
		assertEquals(ListClass.class, manager.getType(key, new ArrayList()));
	}
	
	@Test
	public void testMultipleBoundsListTypeSolver() {
		JGum jgum = new JGum();
		jgum.forClass(Vector.class);
		TypeSolverManager manager = new JGumTypeSolverManager(new JGum());
		Object key = new Object();
		manager.register(key, new MultipleBoundsListTypeSolver());
		try {
			manager.getType(key, new Object());
			fail();
		} catch(UnrecognizedObjectException e) {}
		assertEquals(ListClass.class, manager.getType(key, new Vector())); //trying with a class registered before the bound type solver was added
		assertEquals(ListClass.class, manager.getType(key, new ArrayList())); //trying with a class registered after the bound type solver was added
		try {
			manager.getType(key, new HashSet());
			fail();
		} catch(UnrecognizedObjectException e) {}
	}
	
}
