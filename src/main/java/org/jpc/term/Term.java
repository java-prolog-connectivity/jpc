package org.jpc.term;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.jpc.JpcException;
import org.jpc.engine.visitor.AbstractJpcVisitor;
import org.jpc.engine.visitor.ChangeVariableNameVisitor;
import org.jpc.engine.visitor.CollectVariableNamesVisitor;
import org.jpc.engine.visitor.JpcTermWriterVisitor;
import org.jpc.engine.visitor.ReplaceVariableVisitor;
import org.jpc.util.LogicUtil;


/**
 * A class reifying a logic term
 * DISCLAIMER: In the current version many methods in this class have been copied or adapted from the class jpl.Term in the JPL library.
 * @author scastro
 *
 */
public abstract class Term implements TermAdaptable {

	public static Term newTerm(Object o) {
		Term term = null;
		if(o == null)
			term = Variable.ANONYMOUS_VAR;
		else if(o instanceof TermAdaptable)
			term = ((TermAdaptable)o).asTerm();
		else if(o instanceof Boolean || o instanceof String || o instanceof StringBuilder || o instanceof StringBuffer)
			term = new Atom(o.toString());
		else if(o instanceof Number) {
			if(o instanceof BigDecimal || o instanceof Float || o instanceof Double)
				term = new FloatTerm(((Number)o).doubleValue());
			else
				term = new IntegerTerm(((Number)o).longValue());
		} else if (o instanceof Entry) {
			Term key = newTerm(((Entry)o).getKey());
			Term value = newTerm(((Entry)o).getValue());
			if(key != null && value != null)
				term = new Compound("=", Arrays.<TermAdaptable>asList(key, value));
		} else if(o instanceof Map) {
			term = newTerm(((Map)o).entrySet());
		} else if(o instanceof Enumeration) {
			term = newTerm(Collections.list((Enumeration) o));
		} else if(o instanceof Iterable) {
			term = newTerm(((Iterable)o).iterator());
		} else if(o instanceof Iterator) {
			Iterator it = (Iterator) o;
			List<Term> list = new ArrayList<>();
			while(it.hasNext()) {
				list.add(newTerm(it.next()));
			}
			term = LogicUtil.termsToList(list);
		}
		return term;
	}

	
	/**
	 * Returns the ith argument (counting from 1) of this Compound;
	 * throws an ArrayIndexOutOfBoundsException if i is inappropriate.
	 * 
	 * @return the ith argument (counting from 1) of this Compound
	 */
	public Term arg(int i) {
		return args().get(i-1);
	}
	
	public List<Term> args() {
		return Collections.emptyList();
	}
	
	/**
	 * Returns the arity (1+) of this Term.
	 * 
	 * @return the arity (1+) of this Term. Returns 0 if the terms is not an instance of Compound
	 */
	public int arity() {
		return args().size();
	}
	
	public boolean hasFunctor(String name, int arity) {
		return hasFunctor(new Atom(name), arity);
	}
	
	public abstract boolean hasFunctor(TermAdaptable nameTermObject, int arity);

	
	/**
	 * whether this Term represents an atom
	 * 
	 * @return whether this Term represents an atom
	 */
	public boolean isAtom() {
		return this instanceof Atom;
	}

	/**
	 * whether this Term represents a compound term
	 * 
	 * @return whether this Term represents a compound atom
	 */
	public boolean isCompound() {
		return this instanceof Compound;
	}

	public boolean isNumber() {
		return isInteger() || isFloat();
	}
	
	/**
	 * whether this Term represents an atom
	 * 
	 * @return whether this Term represents an atom
	 */
	public boolean isFloat() {
		return this instanceof FloatTerm;
	}

	/**
	 * whether this Term represents an atom
	 * 
	 * @return whether this Term represents an atom
	 */
	public boolean isInteger() {
		return this instanceof IntegerTerm;
	}

	/**
	 * whether this Term is a variable
	 * 
	 * @return whether this Term is a variable
	 */
	public boolean isVariable() {
		return this instanceof Variable;
	}
	
	/**
	 * whether this Term is a list
	 * 
	 * @return whether this Term is a list
	 */
	public boolean isList() {
		try {
			listLength(); //will throw an exception if the list is not well formed
			return true;
		} catch(Exception e) {
			return false;
		}
	}
	
	public boolean isUnification() {
		return hasFunctor("=", 2);
	}
	
	public boolean bound() {
		return getVariablesNames().isEmpty();
	}
	
	public abstract void accept(AbstractJpcVisitor termVisitor);
	
	/**
	 * the length of this list, iff it is one, else an exception is thrown
	 * 
	 * @throws LException
	 * @return the length (as an int) of this list, iff it is one
	 */
	public int listLength() {
		Compound compound = (Compound) this;
		if (compound.hasFunctor(".", 2)) {
			return 1 + compound.arg(2).asTerm().listLength();
		} else if (compound.hasFunctor("[]", 0)) {
			return 0;
		} else {
			throw new JpcException("term" + compound.toString() + "is not a list");
		}
	}
	
	
	

	protected static <T extends TermAdaptable> boolean equals(List<T> t1, List<T> t2) {
		if (t1.size() != t2.size()) {
			return false;
		}
		for (int i = 0; i < t1.size(); ++i) {
			if (!t1.get(i).equals(t2.get(i))) {
				return false;
			}
		}
		return true;
	}
	
	/**
	 * @param   t1  a list of Terms
	 * @param   t2  another list of Terms
	 * @return  true if all of the Terms in the (same-length) lists are pairwise term equivalent
	 */
	protected static boolean termEquivalent(List<? super Term> t1, List<? super Term> t2) {
		if (t1.size() != t2.size()) {
			return false;
		}
		for (int i = 0; i < t1.size(); ++i) {
			Object o1 = t1.get(i);
			Object o2 = t2.get(i);
			if(o1 instanceof TermAdaptable && o2 instanceof TermAdaptable) {
				Term term1 = ((TermAdaptable)o1).asTerm();
				Term term2 = ((TermAdaptable)o1).asTerm();
				if (!term1.termEquivalent(term2)) {
					return false;
				}
			} else
				return false;
		}
		return true;
	}
	
	/**
	 * Test if this object is equivalent to the term representation of the object sent as parameter
	 * This is not testing for equality in the mathematical sense, for example:
	 * 		'new Variable("_").equals(new Variable("_"))'
	 * is false, since both the receiver and the arguments are anonymous variables. But:
	 * 		'new Variable("_").termEquivalent(new Variable("_"))'
	 * is true, since they both have the same term representation
	 * @param termAdaptable
	 * @return
	 */
	public boolean termEquivalent(TermAdaptable termAdaptable) {
		return equals(termAdaptable.asTerm());
	}
	
	/**
	 * Converts a list of Terms to a String.
	 * 
	 * @param   args    An array of Terms to convert
	 * @return  String representation of a list of Terms
	 */
	public static String toString(TermAdaptable... termObjects) {
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < termObjects.length; ++i) {
			sb.append(termObjects[i].asTerm().toString());
			if (i != termObjects.length - 1) {
				sb.append(", ");
			}
		}
		return sb.toString();
	}
	
	public static <T extends TermAdaptable>String toString(List<T> termAdapters) {
		return toString(termAdapters.toArray(new TermAdaptable[]{}));
	}


	public Term replaceVariables(Map<String, TermAdaptable> map) {
		JpcTermWriterVisitor termWriter = new JpcTermWriterVisitor();
		ReplaceVariableVisitor adapterVisitor = new ReplaceVariableVisitor(termWriter, map);
		accept(adapterVisitor);
		return termWriter.terms().get(0);
	}
	
	public Term changeVariablesNames(Map<String, String> map) {
		JpcTermWriterVisitor termWriter = new JpcTermWriterVisitor();
		ChangeVariableNameVisitor adapterVisitor = new ChangeVariableNameVisitor(termWriter, map);
		accept(adapterVisitor);
		return termWriter.terms().get(0);
	}
	
	public List<String> getVariablesNames() {
		CollectVariableNamesVisitor visitor = new CollectVariableNamesVisitor();
		accept(visitor);
		return visitor.getVariableNames();
	}
	
	public boolean hasVariable(String variableName) {
		return getVariablesNames().contains(variableName);
	}
	
	public List<String> nonAnonymousVariablesNames() {
		List<String> nonAnonymousVariablesNames = new ArrayList<>();
		for(String variableName : getVariablesNames()) {
			if(!Variable.isAnonymousVariableName(variableName))
				nonAnonymousVariablesNames.add(variableName);
		}
		return nonAnonymousVariablesNames;
	}
	
	public Term asTerm() {
		return this;
	}
	
	public static <T extends TermAdaptable> List<Term> asTerms(List<T> termObjects) {
		List<Term> terms = new ArrayList<>();
		for(TermAdaptable termObject : termObjects)
			terms.add(termObject.asTerm());
		return terms;
	}
	
}
