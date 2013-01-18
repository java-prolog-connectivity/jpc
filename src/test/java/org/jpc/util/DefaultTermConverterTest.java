package org.jpc.util;

import static java.util.Arrays.asList;
import static org.jpc.term.ListTerm.listTerm;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import org.jpc.converter.fromterm.DefaultFromTermConverter;
import org.jpc.converter.toterm.DefaultToTermConverter;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.Term;
import org.jpc.term.TermConvertable;
import org.jpc.term.Variable;
import org.junit.Test;

public class DefaultTermConverterTest {

	// *** TERM TO OBJECTS TESTS ***
	
	private Term asTerm(Object o) {
		return new DefaultToTermConverter().apply(o);
	}
	
	@Test
	public void testNullToTerm() {
		assertTrue(new Variable("_").termEquals(asTerm(null)));
	}
	
	@Test
	public void testTermToTerm() {
		assertEquals(new Atom("x"), asTerm(new Atom("x")));
	}
	
	@Test
	public void testBooleanToTerm() {
		assertEquals(new Atom("true"), asTerm(true));
		assertEquals(new Atom("false"), asTerm(false));
	}
	
	@Test
	public void testStringToTerm() {
		assertEquals(new Atom("apple"), asTerm("apple"));
		assertEquals(new Atom("apple"), asTerm(new StringBuffer("apple")));
		assertEquals(new Atom("apple"), asTerm(new StringBuilder("apple")));
	}
	
	@Test
	public void testCharToTerm() {
		char a = 'a';
		assertEquals(new Atom("a"), asTerm(a));
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
		
		assertEquals(new IntegerTerm(10), asTerm(aByte));
		assertEquals(new IntegerTerm(10), asTerm(aShort));
		assertEquals(new IntegerTerm(10), asTerm(anInt));
		assertEquals(new IntegerTerm(10), asTerm(aLong));
		assertEquals(new IntegerTerm(10), asTerm(ai));
		assertEquals(new IntegerTerm(10), asTerm(al));
		assertEquals(new IntegerTerm(10), asTerm(bi));
	}
	
	@Test
	public void testNumberToFloatTerm() {
		float aFloat = 10.5f;
		double aDouble = 10.5;
		BigDecimal bd = new BigDecimal(10.5);
		
		assertEquals(new FloatTerm(10.5), asTerm(aFloat));
		assertEquals(new FloatTerm(10.5), asTerm(aDouble));
		assertEquals(new FloatTerm(10.5), asTerm(bd));
	}
	
	@Test
	public void testEntryToTerm() {
		Map.Entry<String, Integer> entry = new AbstractMap.SimpleEntry<>("apple", 10);
		assertEquals(new Compound("-", asList(new Atom("apple"), new IntegerTerm(10))), asTerm(entry));
	}
	
	@Test
	public void testMapToTerm() {
		Map<String, Integer> map = new LinkedHashMap<String, Integer>() {{ //LinkedHashMap to preserve insertion order
			put("apple", 10);
			put("orange", 20);
		}};
		List<Term> listTerm = asTerm(map).asList();
		assertEquals(2, listTerm.size());
		assertEquals(new Compound("-", asList(new Atom("apple"), new IntegerTerm(10))), listTerm.get(0));
		assertEquals(new Compound("-", asList(new Atom("orange"), new IntegerTerm(20))), listTerm.get(1));
	}
	
	@Test
	public void testArrayToTerm() {
		Term nonEmptyArray = asTerm(new Object[]{"apple", 10});
		assertEquals(new Compound(".", asList(new Atom("apple"), 
				new Compound(".", asList(new IntegerTerm(10), 
				new Atom("[]"))))), nonEmptyArray);
		assertEquals(asTerm(new Object[]{}), new Atom("[]"));
	}
	
	@Test
	public void testListToTerm() {
		Term nonEmptyList = asTerm(new ArrayList() {{ 
			add("apple");
			add(10);
		}});
		assertEquals(new Compound(".", asList(new Atom("apple"), 
				new Compound(".", asList(new IntegerTerm(10), 
				new Atom("[]"))))), nonEmptyList);
		assertEquals(asTerm(new ArrayList()), new Atom("[]"));
	}

	
	// *** OBJECT TO TERM TESTS ***
	
	private Object asObject(Term t) {
		return new DefaultFromTermConverter().apply(t);
	}
	
	@Test
	public void testNullTermToObject() {
		assertNull(asObject(new Variable("X")));
	}
	
	@Test
	public void testTermToString() {
		assertEquals("apple", asObject(new Atom("apple")));
	}
	
	@Test
	public void testTermToBoolean() {
		assertEquals(true, asObject(new Atom("true")));
		assertEquals(false, asObject(new Atom("false")));
	}
	
	@Test
	public void testTermToInt() {
		assertEquals(10, asObject(new IntegerTerm(10)));
	}
	
	@Test
	public void testTermToDouble() {
		assertEquals(10.5, asObject(new FloatTerm(10.5)));
	}
	
	@Test
	public void testTermToEntry() {
		Term entryTerm = new Compound("=", asList(new Atom("apple"), new IntegerTerm(10)));
		Map.Entry<String, Integer> entry = new AbstractMap.SimpleEntry<>("apple", 10);
		assertEquals(entry.getKey(), ((Map.Entry)asObject(entryTerm)).getKey());
		assertEquals(entry.getValue(), ((Map.Entry)asObject(entryTerm)).getValue());
	}
	
	@Test
	public void testTermToMap() {
		Compound c1 = new Compound("=", asList(new Atom("apple"), new IntegerTerm(10)));
		Compound c2 = new Compound("=", asList(new Atom("orange"), new IntegerTerm(20)));
		TermConvertable list = listTerm(c1, c2);
		Map map = (Map) asObject(list.asTerm());
		assertEquals(2, map.size());
		assertEquals(map.get("apple"), 10);
		assertEquals(map.get("orange"), 20);
	}
	
	@Test
	public void testTermToList() {
		TermConvertable listTerm = listTerm(new Atom("apple"), new Variable("X"));
		List list = (List) asObject(listTerm.asTerm());
		assertEquals(2, list.size());
		assertEquals(list.get(0), "apple");
		assertEquals(list.get(1), null);
	}
}
