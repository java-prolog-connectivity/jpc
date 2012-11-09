package org.jpc.term;

import static org.jpc.util.LogicUtil.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.jpc.JpcException;
import org.jpc.engine.visitor.AbstractTermVisitor;
import org.jpc.engine.visitor.ChangeVariableNameVisitor;
import org.jpc.engine.visitor.CollectVariableNamesVisitor;
import org.jpc.engine.visitor.JpcTermWriterVisitor;
import org.jpc.engine.visitor.ReplaceVariableVisitor;
import org.jpc.util.DefaultTermAdapter;


/**
 * @author scastro
 *
 */
public abstract class AbstractTerm implements Term {
	
	/* (non-Javadoc)
	 * @see org.jpc.term.Term#arg(int)
	 */
	@Override
	public Term arg(int i) {
		return args().get(i-1);
	}
	
	/* (non-Javadoc)
	 * @see org.jpc.term.Term#args()
	 */
	@Override
	public List<Term> args() {
		return Collections.emptyList();//assuming no arguments by default
	}
	
	/* (non-Javadoc)
	 * @see org.jpc.term.Term#arity()
	 */
	@Override
	public int arity() {
		return args().size();
	}
	
	@Override
	public boolean hasFunctor(String nameTermObject, int arity) {
		return hasFunctor(new Atom(nameTermObject), arity);
	}
	
	/* (non-Javadoc)
	 * @see org.jpc.term.Term#hasFunctor(org.jpc.term.TermAdaptable, int)
	 */
	@Override
	public abstract boolean hasFunctor(TermAdaptable nameTermObject, int arity);

	
	/* (non-Javadoc)
	 * @see org.jpc.term.Term#isAtom()
	 */
	@Override
	public boolean isAtom() {
		return this instanceof Atom;
	}

	/* (non-Javadoc)
	 * @see org.jpc.term.Term#isCompound()
	 */
	@Override
	public boolean isCompound() {
		return this instanceof Compound;
	}

	/* (non-Javadoc)
	 * @see org.jpc.term.Term#isNumber()
	 */
	@Override
	public boolean isNumber() {
		return isInteger() || isFloat();
	}
	
	/* (non-Javadoc)
	 * @see org.jpc.term.Term#isFloat()
	 */
	@Override
	public boolean isFloat() {
		return this instanceof FloatTerm;
	}

	/* (non-Javadoc)
	 * @see org.jpc.term.Term#isInteger()
	 */
	@Override
	public boolean isInteger() {
		return this instanceof IntegerTerm;
	}

	/* (non-Javadoc)
	 * @see org.jpc.term.Term#isVariable()
	 */
	@Override
	public boolean isVariable() {
		return this instanceof Variable;
	}
	
	/* (non-Javadoc)
	 * @see org.jpc.term.Term#isList()
	 */
	@Override
	public boolean isList() {
		try {
			listLength(); //will throw an exception if the list is not well formed
			return true;
		} catch(Exception e) {
			return false;
		}
	}
	
	/* (non-Javadoc)
	 * @see org.jpc.term.Term#isUnification()
	 */
	@Override
	public boolean isUnification() {
		return hasFunctor("=", 2);
	}
	
	/* (non-Javadoc)
	 * @see org.jpc.term.Term#bound()
	 */
	@Override
	public boolean isBound() {
		return getVariablesNames().isEmpty();
	}
	
	/* (non-Javadoc)
	 * @see org.jpc.term.Term#accept(org.jpc.engine.visitor.AbstractJpcVisitor)
	 */
	@Override
	public abstract void accept(AbstractTermVisitor termVisitor);
	
	/* (non-Javadoc)
	 * @see org.jpc.term.Term#listLength()
	 */
	@Override
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
	
	
	



	
	/**
	 * @param   t1  a list of Terms
	 * @param   t2  another list of Terms
	 * @return  true if all of the Terms in the (same-length) lists are pairwise term equivalent
	 */
	protected static <T extends TermAdaptable> boolean termEquals(List<T> t1, List<T> t2) {
		if (t1.size() != t2.size()) {
			return false;
		}
		for (int i = 0; i < t1.size(); ++i) {
			TermAdaptable o1 = t1.get(i);
			TermAdaptable o2 = t2.get(i);
			if(!o1.asTerm().termEquals(o2.asTerm()))
				return false;
		}
		return true;
	}
	
	/* (non-Javadoc)
	 * @see org.jpc.term.Term#termEquivalent(org.jpc.term.TermAdaptable)
	 */
	@Override
	public boolean termEquals(TermAdaptable o) {
		return equals(o.asTerm());
	}


	/* (non-Javadoc)
	 * @see org.jpc.term.Term#replaceVariables(java.util.Map)
	 */
	@Override
	public Term replaceVariables(Map<String, TermAdaptable> map) {
		JpcTermWriterVisitor termWriter = new JpcTermWriterVisitor();
		ReplaceVariableVisitor adapterVisitor = new ReplaceVariableVisitor(termWriter, map);
		accept(adapterVisitor);
		return termWriter.terms().get(0);
	}
	
	/* (non-Javadoc)
	 * @see org.jpc.term.Term#changeVariablesNames(java.util.Map)
	 */
	@Override
	public Term changeVariablesNames(Map<String, String> map) {
		JpcTermWriterVisitor termWriter = new JpcTermWriterVisitor();
		ChangeVariableNameVisitor adapterVisitor = new ChangeVariableNameVisitor(termWriter, map);
		accept(adapterVisitor);
		return termWriter.terms().get(0);
	}
	
	/* (non-Javadoc)
	 * @see org.jpc.term.Term#getVariablesNames()
	 */
	@Override
	public List<String> getVariablesNames() {
		CollectVariableNamesVisitor visitor = new CollectVariableNamesVisitor();
		accept(visitor);
		return visitor.getVariableNames();
	}
	
	/* (non-Javadoc)
	 * @see org.jpc.term.Term#hasVariable(java.lang.String)
	 */
	@Override
	public boolean hasVariable(String variableName) {
		return getVariablesNames().contains(variableName);
	}
	
	/* (non-Javadoc)
	 * @see org.jpc.term.Term#nonAnonymousVariablesNames()
	 */
	@Override
	public List<String> nonAnonymousVariablesNames() {
		List<String> nonAnonymousVariablesNames = new ArrayList<>();
		for(String variableName : getVariablesNames()) {
			if(!Variable.isAnonymousVariableName(variableName))
				nonAnonymousVariablesNames.add(variableName);
		}
		return nonAnonymousVariablesNames;
	}
	
	/* (non-Javadoc)
	 * @see org.jpc.term.Term#asTerm()
	 */
	@Override
	public AbstractTerm asTerm() {
		return this;
	}
	
	/**
	 * Converts an array of Terms to a String.
	 * 
	 * @param   args    an array of Terms to convert
	 * @return  String representation of an array of Terms
	 */
	public static <T extends TermAdaptable> String toString(T... termObjects) {
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < termObjects.length; ++i) {
			sb.append(termObjects[i].asTerm().toString());
			if (i != termObjects.length - 1) {
				sb.append(", ");
			}
		}
		return sb.toString();
	}
	
	/**
	 * Converts a list of Terms to a String.
	 * 
	 * @param termAdapters
	 * @return String representation of a list of Terms
	 */
	public static <T extends TermAdaptable> String toString(List<T> termObjects) {
		return toString(termObjects.<TermAdaptable>toArray(new TermAdaptable[]{}));
	}

}
