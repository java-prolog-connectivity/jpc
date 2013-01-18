package org.jpc.converter.fromterm;

import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.jpc.JpcException;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.Term;
import org.jpc.term.Variable;
import org.jpc.util.LogicUtil;

import com.google.common.base.Predicate;
import com.google.common.collect.Iterators;

public class DefaultFromTermConverter implements FromTermConverter<Object> {

	public Object apply(Term term) {
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
		} else if(LogicUtil.isPair(term) || LogicUtil.isUnification(term)) {
			final Compound compound = (Compound) term;
			o = new AbstractMap.SimpleEntry(apply(compound.arg(1)), apply(compound.arg(2)));
		} else if(term.isList()) {
			List<Term> listMembers = term.asList();
			Predicate<Term> isPairOrUnification = new Predicate<Term>() {
				public boolean apply(Term t) {
					return LogicUtil.isPair(t) || LogicUtil.isUnification(t);
				}	
			};
			if(Iterators.all(listMembers.iterator(), isPairOrUnification) && !listMembers.isEmpty()) {
				o = asMap(listMembers);
			} else {
				o = asList(listMembers);
			}
		} else
			throw new JpcException("Impossible to interpret the term " + term + " as an object");
		return o;
	}

	public <T> List<T> asList(Iterable<Term> it) {
		List<T> list = new ArrayList<>();
		for(Term term : it) {
			list.add((T)apply(term));
		}
		return list;
	}

	public <K,V> Map<K,V> asMap(Iterable<Term> it) {
		Map<K,V> map = new HashMap<>();
		for(Term term : it) {
			Object o = apply(term);
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
