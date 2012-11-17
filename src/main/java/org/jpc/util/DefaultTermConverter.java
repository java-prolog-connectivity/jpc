package org.jpc.util;

import static java.util.Arrays.asList;
import static org.jpc.util.LogicUtil.termsToList;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.jpc.JpcException;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.Term;
import org.jpc.term.TermConvertable;
import org.jpc.term.Variable;

import com.google.common.base.Predicate;
import com.google.common.collect.Iterators;

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
		else if(o instanceof Boolean || o instanceof String || o instanceof StringBuilder || o instanceof StringBuffer)
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
			term = asTerm(((Map)o).entrySet());
		} else if(o instanceof Enumeration) {
			term = asTerm(Collections.list((Enumeration) o));
		} else if(o instanceof Iterable) {
			term = asTerm(((Iterable)o).iterator());
		} else if(o instanceof Iterator) {
			term = termsToList(asTerms((Iterator) o));
		}
		else
			throw new JpcException("Impossible to interpret the object " + o + " as a term");
		return term;
	}

	public static Object asObject(Term term) {
		if(term instanceof Variable) {
			return null;
		}
		Object o = null;
		if(term instanceof Atom) {
			Atom atom = (Atom) term;
			String name = atom.name();
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
			o = new Hashtable() {{put(asObject(compound.arg(1)), compound.arg(2));}}.entrySet().toArray()[0];
		} else if(term.isListTerm()) {
			final Compound compound = (Compound) term;
			List<Term> listMembers = LogicUtil.listToTerms(compound);
			
			Predicate<Term> isUnification = new Predicate<Term>() {
				public boolean apply(Term t) {
					return LogicUtil.isUnification(t);
				}	
			};
			if(Iterators.all(listMembers.iterator(), isUnification) && !listMembers.isEmpty()) {
				Map map = new HashMap<>();
				for(Term listMember : listMembers) {
					Entry entry = (Entry) asObject(listMember);
					map.put(entry.getKey(), entry.getValue());
				}
				o = map;
			} else {
				o = asObjects(listMembers.iterator());
			}
		} else
			throw new JpcException("Impossible to interpret the term " + term + " as an object");
		return o;
	}
	
	public static List<Term> asTerms(Iterable it) {
		return asTerms(it.iterator());
	}
	
	public static List<Term> asTerms(Iterator it) {
		List<Term> terms = new ArrayList<>();
		while(it.hasNext())
			terms.add(asTerm(it.next()));
		return terms;
	}
	
	public static List asObjects(Iterable it) {
		return asObjects(it.iterator());
	}
	
	public static List asObjects(Iterator<Term> it) {
		List objects = new ArrayList<>();
		while(it.hasNext())
			objects.add(asObject(it.next()));
		return objects;
	}


	
}
