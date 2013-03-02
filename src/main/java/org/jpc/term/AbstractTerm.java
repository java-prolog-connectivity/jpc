package org.jpc.term;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.jpc.JpcException;
import org.jpc.engine.prolog.PrologEngine;
import org.jpc.salt.JpcTermWriter;
import org.jpc.util.salt.ChangeVariableNameAdapter;
import org.jpc.util.salt.ReplaceVariableAdapter;
import org.jpc.util.salt.VariableNamesCollectorHandler;


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
	
	@Override
	public boolean hasFunctor(boolean nameTermObject, int arity) {
		return hasFunctor(new Atom(Boolean.toString(nameTermObject)), arity);
	}
	
	@Override
	public boolean hasFunctor(double nameTermObject, int arity) {
		return hasFunctor(new FloatTerm(nameTermObject), arity);
	}
	
	@Override
	public boolean hasFunctor(long nameTermObject, int arity) {
		return hasFunctor(new IntegerTerm(nameTermObject), arity);
	}
	
	/* (non-Javadoc)
	 * @see org.jpc.term.Term#hasFunctor(org.jpc.term.TermAdaptable, int)
	 */
	@Override
	public abstract boolean hasFunctor(TermConvertable nameTermObject, int arity);

	
	/* (non-Javadoc)
	 * @see org.jpc.term.Term#isList()
	 */
	@Override
	public boolean isList() {
		return false;
	}
	
	@Override
	public ListTerm asList() {
		throw new UnsupportedOperationException();
	}
	
	/* (non-Javadoc)
	 * @see org.jpc.term.Term#bound()
	 */
	@Override
	public boolean isBound() {
		return getVariablesNames().isEmpty();
	}
	
	/* (non-Javadoc)
	 * @see org.jpc.term.Term#listLength()
	 */
	@Override
	public int listLength() {
		Compound compound = (Compound) this;
		if (compound.hasFunctor(".", 2)) {
			return 1 + compound.arg(2).listLength();
		} else if (compound.hasFunctor("[]", 0)) {
			return 0;
		} else {
			throw new JpcException("term" + compound.toString() + "is not a list");
		}
	}


	/* (non-Javadoc)
	 * @see org.jpc.term.Term#replaceVariables(java.util.Map)
	 */
	@Override
	public Term replaceVariables(Map<String, ? extends TermConvertable> map) {
		JpcTermWriter termWriter = new JpcTermWriter();
		ReplaceVariableAdapter replaceVariableAdapter = new ReplaceVariableAdapter(termWriter, map);
		read(replaceVariableAdapter);
		return termWriter.getTerms().get(0);
	}
	
	/* (non-Javadoc)
	 * @see org.jpc.term.Term#changeVariablesNames(java.util.Map)
	 */
	@Override
	public Term changeVariablesNames(Map<String, String> map) {
		JpcTermWriter termWriter = new JpcTermWriter();
		ChangeVariableNameAdapter changeVariableNameAdapter = new ChangeVariableNameAdapter(termWriter, map);
		read(changeVariableNameAdapter);
		return termWriter.getTerms().get(0);
	}
	
	/* (non-Javadoc)
	 * @see org.jpc.term.Term#getVariablesNames()
	 */
	@Override
	public List<String> getVariablesNames() {
		VariableNamesCollectorHandler variableNamesCollector = new VariableNamesCollectorHandler();
		read(variableNamesCollector);
		return variableNamesCollector.getVariableNames();
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
	
	/* (non-Javadoc)
	 * @see org.jpc.term.Term#termEquivalent(org.jpc.term.TermAdaptable)
	 */
	@Override
	public boolean termEquals(TermConvertable o) {
		return equals(o.asTerm());
	}
	
	public abstract String toString(PrologEngine prologEngine);
	
	
	/**
	 * @param   t1  a list of Terms
	 * @param   t2  another list of Terms
	 * @return  true if all of the Terms in the (same-length) lists are pairwise term equivalent
	 */
	protected static <T extends TermConvertable> boolean termEquals(List<T> t1, List<T> t2) {
		if (t1.size() != t2.size()) {
			return false;
		}
		for (int i = 0; i < t1.size(); ++i) {
			TermConvertable o1 = t1.get(i);
			TermConvertable o2 = t2.get(i);
			if(!o1.asTerm().termEquals(o2.asTerm()))
				return false;
		}
		return true;
	}

	/**
	 * Converts an array of Terms to a String.
	 * 
	 * @param   args    an array of Terms to convert
	 * @return  String representation of an array of Terms
	 */
	public static <T extends TermConvertable> String toString(T... termObjects) {
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
	public static <T extends TermConvertable> String toString(List<T> termObjects) {
		return toString(termObjects.toArray(new TermConvertable[]{}));
	}
	
	public static <T extends TermConvertable> String toString(PrologEngine prologEngine, T... termObjects) {
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < termObjects.length; ++i) {
			sb.append(termObjects[i].asTerm().toString(prologEngine));
			if (i != termObjects.length - 1) {
				sb.append(", ");
			}
		}
		return sb.toString();
	}
	
	public static <T extends TermConvertable> String toString(PrologEngine prologEngine, List<T> termObjects) {
		return toString(prologEngine, termObjects.toArray(new TermConvertable[]{}));
	}

}
