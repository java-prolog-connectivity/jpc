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

import org.jpc.DefaultJpc;
import org.jpc.Jpc;
import org.jpc.converter.typesolver.catalog.MapTypeSolver;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.Term;
import org.jpc.term.Var;
import org.jpc.term.jterm.JTermUtil;
import org.jpc.term.jterm.Serialized;
import org.junit.Assert;
import org.junit.Test;

import com.google.common.reflect.TypeToken;

public class DefaultTermConverterTest {

	private Jpc jpc = new DefaultJpc();
	
	
	// *** OBJECT TO TERM TESTS ***
	
	@Test
	public void testNullToTerm() {
		assertTrue(new Var("_").termEquals(jpc.toTerm(null)));
	}
	
	@Test
	public void testTermToTerm() {
		assertEquals(new Atom("x"), jpc.toTerm(new Atom("x")));
	}
	
	@Test
	public void testBooleanToTerm() {
		assertEquals(Atom.TRUE_TERM, jpc.toTerm(true));
		assertEquals(Atom.FAIL_TERM, jpc.toTerm(false));
	}
	
	@Test
	public void testStringToTerm() {
		assertEquals(new Atom("apple"), jpc.toTerm("apple"));
		assertEquals(new Atom("1"), jpc.toTerm("1"));
		assertEquals(new Atom("1"), jpc.toTerm("1", Atom.class));
		assertEquals(new IntegerTerm(1), jpc.toTerm("1", IntegerTerm.class));
		assertEquals(new FloatTerm(1), jpc.toTerm("1", FloatTerm.class));
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
		assertEquals(new FloatTerm(10), jpc.toTerm(10f));
		assertEquals(new FloatTerm(10.5), jpc.toTerm(aFloat));
		assertEquals(new FloatTerm(10.5), jpc.toTerm(aDouble));
		assertEquals(new FloatTerm(10.5), jpc.toTerm(bd));
	}
	
	@Test
	public void testNumberToAtom() {
		assertEquals(new Atom("1"), jpc.toTerm(1, Atom.class));
		assertEquals(new Atom("1.0"), jpc.toTerm(1D, Atom.class));
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
		Term mapTerm = jpc.toTerm(map);
		List<Term> listTerm = mapTerm.asList();
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
		Term nonEmptyList = jpc.toTerm(asList("apple", 10));
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
	
	@Test
	public void testJTerm() {
		Object o = new Object();
		Compound term = new Compound("x", asList(new Atom(""))); //arbitrary compound that will be associated to an object reference.
		JTermUtil.jTerm(term, o); //associating the compound to a reference.
		assertTrue(o == jpc.fromTerm(term));
		o = null;
		System.gc();
		try {
			jpc.fromTerm(term);
			fail();
		} catch(RuntimeException e) {}
	}
	
	@Test
	public void testJRef() {
		Object o = new Object();
		Term jRef = JTermUtil.jRefTerm(o);
		assertTrue(o == jpc.fromTerm(jRef));
		o = null;
		System.gc();
		try {
			jpc.fromTerm(jRef);
			fail();
		} catch(RuntimeException e) {}
	}
	
	@Test
	public void testJRef2() {
		//s1 and s2 are equals but have different references.
		String s1 = "hello";
		String s2 = new String("hello");
		JTermUtil.jRefTerm(s1);
		Term jRef2 = JTermUtil.jRefTerm(s2);
		String stringFromTerm = jpc.fromTerm(jRef2);
		assertFalse(stringFromTerm == s1);
		assertTrue(stringFromTerm == s2);
	}
	
	@Test
	public void testSerializedTerm() {
		String s = "hello";
		Term term = Serialized.jSerializedTerm(s);
		String s2 = jpc.fromTerm(term);
		assertFalse(s == s2);
		assertEquals(s, s2);
	}
	
	
	// *** TERM TO OBJECTS TESTS ***

	@Test
	public void testTermFromTerm() {
		assertEquals(new Atom("apple"), jpc.fromTerm(new Atom("apple"), Term.class));
	}
	
	@Test
	public void testVariableToNull() {
		assertNull(jpc.fromTerm(new Var("X")));
	}
	
	@Test
	public void testTermToString() {
		assertEquals("apple", jpc.fromTerm(new Atom("apple")));
		assertFalse("true".equals(jpc.fromTerm(new Atom(TRUE))));
		assertTrue("true".equals(jpc.fromTerm(new Atom(TRUE), String.class)));
		assertEquals("123", jpc.fromTerm(new Atom("123")));
		assertEquals("123", jpc.fromTerm(new IntegerTerm(123), String.class));
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
		assertTrue(jpc.fromTerm(new IntegerTerm(10L)).equals(10L));
		assertFalse(jpc.fromTerm(new IntegerTerm(10)).equals(10)); //values in Prolog Integer terms are stored as a Java Long
		assertEquals(10L, jpc.fromTerm(new IntegerTerm(10)));
		assertEquals(10, jpc.fromTerm(new IntegerTerm(10), Integer.class));
		assertEquals(10, jpc.fromTerm(new Atom("10"), Integer.class));
		try{
			jpc.fromTerm(new Atom(TRUE), Integer.class);
			fail();
		} catch(Exception e){}
	}
	
	@Test
	public void testTermToDouble() {
		assertTrue(jpc.fromTerm(new FloatTerm(1D)).equals(1D));
		assertFalse(jpc.fromTerm(new FloatTerm(1F)).equals(1F)); //values in Prolog Float terms are stored as a Java Double
		assertTrue(jpc.fromTerm(new FloatTerm(1F)).equals(1D));
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
		Map map = jpc.fromTerm(listTerm);
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
		Compound c1 = new Compound("#", asList(new Atom("apple"), new IntegerTerm(10))); //the symbol # is not a valid entry separator
		Compound c2 = new Compound("#", asList(new Atom("orange"), new IntegerTerm(20)));
		Term listTerm = listTerm(c1, c2);
		try {
			Object x = jpc.fromTerm(listTerm);
			fail();
		} catch(Exception e) {}
	}
	
	@Test
	public void testTermToList() {
		Term listTerm = listTerm(new Atom("apple"), new Var("X"));
		List list = (List) jpc.fromTerm(listTerm);
		assertEquals(2, list.size());
		assertEquals(list.get(0), "apple");
		assertEquals(list.get(1), null);
	}
	
	@Test
	public void testTermToList2() {
		Compound c1 = new Compound("-", asList(new Atom("apple"), new IntegerTerm(10)));
		Compound c2 = new Compound("-", asList(new Atom("orange"), new IntegerTerm(20)));
		Term listTerm = listTerm(c1, c2);
		List list = jpc.fromTerm(listTerm, List.class);
		assertEquals(2, list.size());
		
		assertEquals("apple", ((Entry)list.get(0)).getKey());
		assertEquals(10L, ((Entry)list.get(0)).getValue());
		assertEquals("orange", ((Entry)list.get(1)).getKey());
		assertEquals(20L, ((Entry)list.get(1)).getValue());
	}
	
	@Test
	public void testTermToGenericList() {
		Term listTerm = listTerm(new Atom("1"), new Atom("2")); //['1','2']
		List list = jpc.fromTerm(listTerm); //no type specified
		assertEquals(list.get(0), "1");
		assertEquals(list.get(1), "2");
		Type type = new TypeToken<List<String>>(){}.getType();
		list = jpc.fromTerm(listTerm, type); //redundant specification of the type
		assertEquals(list.get(0), "1");
		assertEquals(list.get(1), "2");
		type = new TypeToken<List<Integer>>(){}.getType();
		list = jpc.fromTerm(listTerm, type); //indicating that the elements of the list should be integers
		assertEquals(list.get(0), 1);
		assertEquals(list.get(1), 2);
		type = new TypeToken<List<Long>>(){}.getType();
		list = jpc.fromTerm(listTerm, type); //indicating that the elements of the list should be longs
		assertEquals(list.get(0), 1L);
		assertEquals(list.get(1), 2L);
	}
	
	@Test
	public void testTermToObjectArray() {
		Term listTerm = listTerm(new Atom("apple"), new Var("X"));
		Object[] array = jpc.fromTerm(listTerm, Object[].class);
		assertEquals(2, array.length);
		assertEquals(array[0], "apple");
		assertEquals(array[1], null);
	}
	
	@Test
	public void testTermToStringArray() {
		Term listTerm = listTerm(new Atom("apple"), new Var("X"));
		String[] array = jpc.fromTerm(listTerm, String[].class);
		assertEquals(2, array.length);
		assertEquals(array[0], "apple");
		assertEquals(array[1], null);
	}
	
	@Test
	public void testTermToObjectTable() {
		Term term = new Compound(".", asList(
				new Compound(".", asList(new Atom("apple"), 
						new Compound(".", asList(new Var("Var"), 
							new Atom("[]"))))), 
					new Compound(".", asList(
						new Compound(".", asList(new Atom("pears"), 
							new Compound(".", asList(new Var("_"), 
								new Atom("[]"))))), 
					new Atom("[]")))));
		Object[][] table = jpc.fromTerm(term, Object[][].class);
		Assert.assertArrayEquals(table, new String[][]{new String[]{"apple", null}, new String[]{"pears", null}});
	}

	@Test
	public void testTermToStringTable() {
		Term term = new Compound(".", asList(
				new Compound(".", asList(new Atom("apple"), 
						new Compound(".", asList(new Var("Var"), 
							new Atom("[]"))))), 
					new Compound(".", asList(
						new Compound(".", asList(new Atom("pears"), 
							new Compound(".", asList(new Var("_"), 
								new Atom("[]"))))), 
					new Atom("[]")))));
		String[][] table = jpc.fromTerm(term, String[][].class);
		Assert.assertArrayEquals(table, new String[][]{new String[]{"apple", null}, new String[]{"pears", null}});
	}

	@Test
	public void testTermToListOfStringArray() {
		Term term = new Compound(".", asList(
				new Compound(".", asList(new Atom("apple"), 
						new Compound(".", asList(new Var("Var"), 
							new Atom("[]"))))), 
					new Compound(".", asList(
						new Compound(".", asList(new Atom("pears"), 
							new Compound(".", asList(new Var("_"), 
								new Atom("[]"))))), 
					new Atom("[]")))));
		Type type = new TypeToken<List<String[]>>(){}.getType();
		List<String[]> list = jpc.fromTerm(term, type);
		Assert.assertArrayEquals(list.get(0), new String[]{"apple", null});
		Assert.assertArrayEquals(list.get(1), new String[]{"pears", null});
	}
	
}
