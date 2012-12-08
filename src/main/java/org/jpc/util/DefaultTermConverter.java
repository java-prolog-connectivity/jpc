package org.jpc.util;

import static java.util.Arrays.asList;

import java.math.BigDecimal;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.jpc.JpcException;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;
import org.jpc.term.TermConvertable;
import org.jpc.term.Variable;

import com.google.common.base.Predicate;
import com.google.common.collect.Iterators;

/**
 * A convenient utility for default conversions from and to Term types
 * @author sergioc
 *
 */
public class DefaultTermConverter {//implements TermConvertable {

//	private Object o;
////	public DefaultTermConverter(Object o) {
////		this.o = o;
////	}
//	
//	@Override
//	public Term asTerm() {
//		return asTerm(o);
//	}
	
	public static Term asTerm(Object o) {
		Term term = null;
		if(o == null)
			term = Variable.ANONYMOUS_VAR;
		else if(o instanceof TermConvertable)
			term = ((TermConvertable)o).asTerm();
		else if(o instanceof Boolean || o instanceof String || o instanceof StringBuilder || o instanceof StringBuffer || o instanceof Character)
			term = new Atom(o.toString());
		else if(o instanceof Number) {
			if(o instanceof BigDecimal || o instanceof Float || o instanceof Double)
				term = new FloatTerm(((Number)o).doubleValue());
			else
				term = new IntegerTerm(((Number)o).longValue());
		} else if (o instanceof Entry) {
			Term key = asTerm(((Entry)o).getKey());
			Term value = asTerm(((Entry)o).getValue());
			term = new Compound("=", asList(key, value));
		} else if(o instanceof Map) {
			term = asTermList((Map)o).asTerm();
		} else if(o instanceof Enumeration) {
			term = asTermList((Enumeration)o).asTerm();
		} else if(o instanceof Object[]) {
			term = asTermList((Object[])o).asTerm();
		} else if(o instanceof Iterable) {
			term = asTermList((Iterable)o).asTerm();
		} else if(o instanceof Iterator) {
			term = asTermList((Iterator)o).asTerm();
		}
		else
			throw new JpcException("Impossible to interpret the object " + o + " as a term");
		return term;
	}

	public static Object deify(Term term) {
		if(term instanceof Variable) {
			return null;
		}
		Object o = null;
		if(term instanceof Atom) {
			Atom atom = (Atom) term;
			String name = atom.getName();
			if(name.equals("true") || name.equals("false"))
				o = Boolean.valueOf(name);
			else
				o = name;
		} else if(term instanceof FloatTerm) {
			FloatTerm floatTerm = (FloatTerm) term;
			o = floatTerm.doubleValue();
		} else if(term instanceof IntegerTerm) {
			IntegerTerm integerTerm = (IntegerTerm) term;
			o = integerTerm.intValue();
		} else if(LogicUtil.isUnification(term)) {
			final Compound compound = (Compound) term;
			o = new AbstractMap.SimpleEntry(deify(compound.arg(1)), deify(compound.arg(2)));
		} else if(term.isList()) {
			List<Term> listMembers = term.asList();
			Predicate<Term> isUnification = new Predicate<Term>() {
				public boolean apply(Term t) {
					return LogicUtil.isUnification(t);
				}	
			};
			if(Iterators.all(listMembers.iterator(), isUnification) && !listMembers.isEmpty()) {
				o = asDeifiedMap(listMembers);
			} else {
				o = asDeifiedList(listMembers);
			}
		} else
			throw new JpcException("Impossible to interpret the term " + term + " as an object");
		return o;
	}
	
	public static ListTerm asTermList(Map map) {
		return asTermList(map.entrySet());
	}
	
	public static ListTerm asTermList(Object[] objects) {
		return asTermList(asList(objects));
	}
	
	public static ListTerm asTermList(Enumeration en) {
		return asTermList(Collections.list(en));
	}
	
	public static ListTerm asTermList(Iterable it) {
		return asTermList(it.iterator());
	}
	
	public static ListTerm asTermList(Iterator it) {
		ListTerm terms = new ListTerm();
		while(it.hasNext())
			terms.add(asTerm(it.next()));
		return terms;
	}
	
	public static <T> List<T> asDeifiedList(Iterable<Term> it) {
		List<T> list = new ArrayList<>();
		for(Term term : it) {
			list.add((T)deify(term));
		}
		return list;
	}

	public static <K,V> Map<K,V> asDeifiedMap(Iterable<Term> it) {
		Map<K,V> map = new HashMap<>();
		for(Term term : it) {
			Object o = deify(term);
			Entry<K,V> entry = null;
			try {
				entry = (Entry) o;
			} catch(ClassCastException e) {
				throw new JpcException("The term " + term + " cannot be converted to a map entry");
			}
			map.put(entry.getKey(), entry.getValue());
		}
		return map;
	}

}
