package org.jpc.converter;

import static java.util.Arrays.asList;
import static org.jpc.engine.prolog.PrologConstants.FAIL;
import static org.jpc.engine.prolog.PrologConstants.FALSE;
import static org.jpc.engine.prolog.PrologConstants.TRUE;
import static org.jpc.term.ListTerm.listTerm;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import org.jpc.Jpc;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;
import org.jpc.term.Variable;
import org.jpc.typesolver.MapTypeSolver;
import org.junit.Assert;
import org.junit.Test;

import com.google.common.reflect.TypeToken;

public class DefaultTermConverterTest {

	private Jpc jpc = new Jpc();
	
	// *** TERM TO OBJECTS TESTS ***
	
	@Test
	public void testNullToTerm() {
		assertTrue(new Variable("_").termEquals(jpc.toTerm(null)));
	}
	
	@Test
	public void testTermToTerm() {
		assertEquals(new Atom("x"), jpc.toTerm(new Atom("x")));
	}
	
	@Test
	public void testBooleanToTerm() {
		assertEquals(Atom.TRUE_TERM, jpc.toTerm(true));
		assertEquals(Atom.FALSE_TERM, jpc.toTerm(false));
	}
	
	@Test
	public void testStringToTerm() {
		assertEquals(new Atom("apple"), jpc.toTerm("apple"));
		assertEquals(new Atom("apple"), jpc.toTerm(new StringBuffer("apple")));
		assertEquals(new Atom("apple"), jpc.toTerm(new StringBuilder("apple")));
	}
	

	@Test
	public void testCharToTerm() {
		char a = 'a';
		assertEquals(new Atom("a"), jpc.toTerm(a));
	}
	
	@Test
	public void testNumberToIntegerTerm() {
		byte aByte = 10;
		short aShort = 10;
		int anInt = 10;
		long aLong = 10;
		AtomicInteger ai = new AtomicInteger(10);
		AtomicLong al = new AtomicLong(10);
		BigInteger bi = BigInteger.TEN;
		
		assertEquals(new IntegerTerm(10), jpc.toTerm(aByte));
		assertEquals(new IntegerTerm(10), jpc.toTerm(aShort));
		assertEquals(new IntegerTerm(10), jpc.toTerm(anInt));
		assertEquals(new IntegerTerm(10), jpc.toTerm(aLong));
		assertEquals(new IntegerTerm(10), jpc.toTerm(ai));
		assertEquals(new IntegerTerm(10), jpc.toTerm(al));
		assertEquals(new IntegerTerm(10), jpc.toTerm(bi));
	}
	
	@Test
	public void testNumberToFloatTerm() {
		float aFloat = 10.5f;
		double aDouble = 10.5;
		BigDecimal bd = new BigDecimal(10.5);
		
		assertEquals(new FloatTerm(10.5), jpc.toTerm(aFloat));
		assertEquals(new FloatTerm(10.5), jpc.toTerm(aDouble));
		assertEquals(new FloatTerm(10.5), jpc.toTerm(bd));
	}
	
	@Test
	public void testEntryToTerm() {
		Map.Entry<String, Integer> entry = new AbstractMap.SimpleEntry<>("apple", 10);
		assertEquals(new Compound(MapTypeSolver.DEFAULT_MAP_ENTRY_SEPARATOR, asList(new Atom("apple"), new IntegerTerm(10))), jpc.toTerm(entry));
	}
	
	@Test
	public void testMapToTerm() {
		Map<String, Integer> map = new LinkedHashMap<String, Integer>() {{ //LinkedHashMap to preserve insertion order
			put("apple", 10);
			put("orange", 20);
		}};
		List<Term> listTerm = jpc.toTerm(map).asList();
		assertEquals(2, listTerm.size());
		assertEquals(new Compound(MapTypeSolver.DEFAULT_MAP_ENTRY_SEPARATOR, asList(new Atom("apple"), new IntegerTerm(10))), listTerm.get(0));
		assertEquals(new Compound(MapTypeSolver.DEFAULT_MAP_ENTRY_SEPARATOR, asList(new Atom("orange"), new IntegerTerm(20))), listTerm.get(1));
	}
	
	@Test
	public void testEmptyArrayToTerm() {
		assertEquals(jpc.toTerm(new Object[]{}), new Atom("[]"));
	}
	
	@Test
	public void testArrayToTerm() {
		Term nonEmptyArray = jpc.toTerm(new Object[]{"apple", 10});
		assertEquals(
				new Compound(".", asList(new Atom("apple"), 
					new Compound(".", asList(new IntegerTerm(10), 
						new Atom("[]")))))
		, nonEmptyArray);
	}
	
	@Test
	public void testTableToTerm() {
		Term table = jpc.toTerm(
			new Object[][]{
				new Object[]{"apple", 10},
				new Object[]{"pears", 10.5}
			});
		assertEquals(new Compound(".", asList(
				new Compound(".", asList(new Atom("apple"), 
					new Compound(".", asList(new IntegerTerm(10), 
						new Atom("[]"))))), 
				new Compound(".", asList(
					new Compound(".", asList(new Atom("pears"), 
						new Compound(".", asList(new FloatTerm(10.5), 
							new Atom("[]"))))), 
				new Atom("[]"))))), table);
	}
	
	@Test
	public void testListToTerm() {
		Term nonEmptyList = jpc.toTerm(new ArrayList() {{ 
			add("apple");
			add(10);
		}});
		assertEquals(new Compound(".", asList(new Atom("apple"), 
				new Compound(".", asList(new IntegerTerm(10), 
				new Atom("[]"))))), nonEmptyList);
		assertEquals(jpc.toTerm(new ArrayList()), new Atom("[]"));
	}

	@Test
	public void testEnumerationToTerm() {
		Enumeration enumeration = Collections.enumeration(new ArrayList() {{ 
			add("apple");
			add(10);
		}});
		Term nonEmptyList = jpc.toTerm(enumeration);
		assertEquals(new Compound(".", asList(new Atom("apple"), 
				new Compound(".", asList(new IntegerTerm(10), 
				new Atom("[]"))))), nonEmptyList);
		assertEquals(jpc.toTerm(new ArrayList()), new Atom("[]"));
	}
	
	
	// *** OBJECT TO TERM TESTS ***

	@Test
	public void testVariableToNull() {
		assertNull(jpc.fromTerm(new Variable("X")));
	}
	
	@Test
	public void testTermToString() {
		assertEquals("apple", jpc.fromTerm(new Atom("apple")));
		assertFalse("true".equals(jpc.fromTerm(new Atom(TRUE))));
		assertTrue("true".equals(jpc.fromTerm(new Atom(TRUE), String.class)));
		assertEquals("123", jpc.fromTerm(new Atom("123")));
		assertEquals(123, jpc.fromTerm(new Atom("123"), Integer.class));
	}
	
	@Test
	public void testTermToChar() {
		assertEquals('a', jpc.fromTerm(new Atom("a"), Character.class));
		assertEquals('1', jpc.fromTerm(new Atom("1"), Character.class));
		try {
			jpc.fromTerm(new Atom("ab"), Character.class);
			fail();
		} catch(Exception e) {}
		
	}
	
	@Test
	public void testTermToBoolean() {
		assertEquals(true, jpc.fromTerm(new Atom(TRUE)));
		assertEquals(false, jpc.fromTerm(new Atom(FAIL)));
		assertEquals(false, jpc.fromTerm(new Atom(FALSE)));
		assertEquals(true, jpc.fromTerm(new Atom(TRUE), Boolean.class));
		assertEquals(false, jpc.fromTerm(new Atom(FAIL), Boolean.class));
		assertEquals(false, jpc.fromTerm(new Atom(FALSE), Boolean.class));
		assertEquals(true, jpc.fromTerm(new Atom(TRUE), Object.class));
		assertEquals(false, jpc.fromTerm(new Atom(FAIL), Object.class));
		assertEquals(false, jpc.fromTerm(new Atom(FALSE), Object.class));
	}
	
	@Test
	public void testTermToInt() {
		assertEquals(10L, jpc.fromTerm(new IntegerTerm(10)));
		assertEquals(10, jpc.fromTerm(new IntegerTerm(10), Integer.class));
		try{
			jpc.fromTerm(new Atom(TRUE), Integer.class);
			fail();
		} catch(Exception e){}
	}
	
	@Test
	public void testTermToDouble() {
		assertEquals(10.5, jpc.fromTerm(new FloatTerm(10.5)));
	}

	@Test
	public void testTermToEntry() {
		Map.Entry<String, Long> entry = new AbstractMap.SimpleEntry<>("apple", 10L);
		Term entryTerm = new Compound("=", asList(new Atom("apple"), new IntegerTerm(10)));
		assertEquals(entry, jpc.fromTerm(entryTerm));
		entryTerm = new Compound("-", asList(new Atom("apple"), new IntegerTerm(10)));
		assertEquals(entry, jpc.fromTerm(entryTerm));
		entryTerm = new Compound("$", asList(new Atom("apple"), new IntegerTerm(10)));
		try {
			assertEquals(entry, jpc.fromTerm(entryTerm, Entry.class));
			fail();
		} catch(Exception e){}
		
	}

	@Test
	public void testTermToMap() {
		Compound c1 = new Compound("-", asList(new Atom("apple"), new IntegerTerm(10)));
		Compound c2 = new Compound("-", asList(new Atom("orange"), new IntegerTerm(20)));
		Term listTerm = listTerm(c1, c2);
		Map map = (Map) jpc.fromTerm(listTerm);
		assertEquals(2, map.size());
		assertEquals(map.get("apple"), 10L);
		assertEquals(map.get("orange"), 20L);
	}
	
	@Test
	public void testTermToMap2() {
		Compound c1 = new Compound("=", asList(new Atom("apple"), new IntegerTerm(10)));
		Compound c2 = new Compound("=", asList(new Atom("orange"), new IntegerTerm(20)));
		Term listTerm = listTerm(c1, c2);
		Map map = (Map) jpc.fromTerm(listTerm);
		assertEquals(2, map.size());
		assertEquals(map.get("apple"), 10L);
		assertEquals(map.get("orange"), 20L);
	}
	
	@Test
	public void testTermToMap3() {
		Compound c1 = new Compound("#", asList(new Atom("apple"), new IntegerTerm(10)));
		Compound c2 = new Compound("#", asList(new Atom("orange"), new IntegerTerm(20)));
		Term listTerm = listTerm(c1, c2);
		try {
			jpc.fromTerm(listTerm);
			fail();
		} catch(Exception e) {}
	}
	
	@Test
	public void testTermToList() {
		Term listTerm = listTerm(new Atom("apple"), new Variable("X"));
		List list = (List) jpc.fromTerm(listTerm);
		assertEquals(2, list.size());
		assertEquals(list.get(0), "apple");
		assertEquals(list.get(1), null);
	}
	
	@Test
	public void testTermToObjectArray() {
		Term listTerm = listTerm(new Atom("apple"), new Variable("X"));
		Object[] array = jpc.fromTerm(listTerm, Object[].class);
		assertEquals(2, array.length);
		assertEquals(array[0], "apple");
		assertEquals(array[1], null);
	}
	
	@Test
	public void testTermToStringArray() {
		Term listTerm = listTerm(new Atom("apple"), new Variable("X"));
		String[] array = jpc.fromTerm(listTerm, String[].class);
		assertEquals(2, array.length);
		assertEquals(array[0], "apple");
		assertEquals(array[1], null);
	}
	
	@Test
	public void testTermToObjectTable() {
		Term term = new Compound(".", asList(
				new Compound(".", asList(new Atom("apple"), 
						new Compound(".", asList(new Variable("Var"), 
							new Atom("[]"))))), 
					new Compound(".", asList(
						new Compound(".", asList(new Atom("pears"), 
							new Compound(".", asList(new Variable("_"), 
								new Atom("[]"))))), 
					new Atom("[]")))));
		Object[][] table = jpc.fromTerm(term, Object[][].class);
		Assert.assertArrayEquals(table, new String[][]{new String[]{"apple", null}, new String[]{"pears", null}});
	}

	@Test
	public void testTermToStringTable() {
		Term term = new Compound(".", asList(
				new Compound(".", asList(new Atom("apple"), 
						new Compound(".", asList(new Variable("Var"), 
							new Atom("[]"))))), 
					new Compound(".", asList(
						new Compound(".", asList(new Atom("pears"), 
							new Compound(".", asList(new Variable("_"), 
								new Atom("[]"))))), 
					new Atom("[]")))));
		String[][] table = jpc.fromTerm(term, String[][].class);
		Assert.assertArrayEquals(table, new String[][]{new String[]{"apple", null}, new String[]{"pears", null}});
	}

	@Test
	public void testTermToListOfStringArray() {
		Term term = new Compound(".", asList(
				new Compound(".", asList(new Atom("apple"), 
						new Compound(".", asList(new Variable("Var"), 
							new Atom("[]"))))), 
					new Compound(".", asList(
						new Compound(".", asList(new Atom("pears"), 
							new Compound(".", asList(new Variable("_"), 
								new Atom("[]"))))), 
					new Atom("[]")))));
		Type type = new TypeToken<List<String[]>>(){}.getType();
		List<String[]> list = jpc.fromTerm(term, type);
		Assert.assertArrayEquals(list.get(0), new String[]{"apple", null});
		Assert.assertArrayEquals(list.get(1), new String[]{"pears", null});
	}
	
}