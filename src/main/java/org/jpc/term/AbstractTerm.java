package org.jpc.term;

import static org.jpc.engine.prolog.PrologConstants.ANONYMOUS_VAR_NAME;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.jpc.JpcException;
import org.jpc.converter.TermConvertable;
import org.jpc.salt.JpcTermWriter;
import org.jpc.util.salt.ChangeVariableNameAdapter;
import org.jpc.util.salt.ReplaceVariableAdapter;
import org.jpc.util.salt.VariableNamesCollectorHandler;


/**
 * @author scastro
 *
 */
public abstract class AbstractTerm implements Term, TermConvertable {
	
	@Override
	public boolean isHilog() {
		return false;
	}
	
	@Override
	public Term arg(int i) {
		return getArgs().get(i-1);
	}
	
	@Override
	public List<Term> getArgs() {
		return Collections.emptyList(); //assuming no arguments by default
	}
	
	@Override
	public int arity() {
		return getArgs().size();
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
	public abstract boolean hasFunctor(Term nameTermObject, int arity);

	
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
	
	@Override
	public boolean isBound() {
		return getVariablesNames().isEmpty();
	}
	
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

	@Override
	public Term replaceVariables(Map<String, ? extends Term> map) {
		JpcTermWriter termWriter = new JpcTermWriter();
		ReplaceVariableAdapter replaceVariableAdapter = new ReplaceVariableAdapter(termWriter, map);
		read(replaceVariableAdapter);
		return termWriter.getTerms().get(0);
	}
	
	@Override
	public Term changeVariablesNames(Map<String, String> map) {
		JpcTermWriter termWriter = new JpcTermWriter();
		ChangeVariableNameAdapter changeVariableNameAdapter = new ChangeVariableNameAdapter(termWriter, map);
		read(changeVariableNameAdapter);
		return termWriter.getTerms().get(0);
	}
	
	@Override
	public boolean hasVariable(String variableName) {
		return getVariablesNames().contains(variableName);
	}
	
	@Override
	public List<Variable> getVariables() {
		return Variable.asVariables(getVariablesNames());
	}
	
	@Override
	public List<String> getVariablesNames() {
		VariableNamesCollectorHandler variableNamesCollector = new VariableNamesCollectorHandler();
		read(variableNamesCollector);
		return variableNamesCollector.getVariableNames();
	}
	
	@Override
	public List<Variable> getNonAnonymousVariables() {
		return Variable.asVariables(getNamedVariablesNames());
	}

	@Override
	public List<String> getNonAnonymousVariablesNames() {
		List<String> nonAnonymousVariablesNames = new ArrayList<>();
		for(String variableName : getVariablesNames()) {
			if(!Variable.isAnonymousVariableName(variableName))
				nonAnonymousVariablesNames.add(variableName);
		}
		return nonAnonymousVariablesNames;
	}
	
	@Override
	public List<Variable> getNamedVariables() {
		return Variable.asVariables(getNamedVariablesNames());
	}
	
	@Override
	public List<String> getNamedVariablesNames() {
		List<String> namedVariablesNames = new ArrayList<>();
		for(String variableName : getVariablesNames()) {
			if(!variableName.equals(ANONYMOUS_VAR_NAME))
				namedVariablesNames.add(variableName);
		}
		return namedVariablesNames;
	}
	
	@Override
	public AbstractTerm asTerm() {
		return this;
	}
	
	@Override
	public boolean termEquals(Term t) {
		return equals(t);
	}
	
	@Override
	public String toString() {
		return toEscapedString();
	}
	
	
	
	/**
	 * @param   list1  a list of Terms
	 * @param   list2  another list of Terms
	 * @return  true if all of the Terms in the (same-length) lists are pairwise term equivalent
	 */
	public static boolean termEquals(List<? extends Term> list1, List<? extends Term> list2) {
		if (list1.size() != list2.size()) {
			return false;
		}
		for (int i = 0; i < list1.size(); ++i) {
			Term term1 = list1.get(i);
			Term term2 = list2.get(i);
			if(!term1.termEquals(term2))
				return false;
		}
		return true;
	}

	/**
	 * Converts an array of Terms to its escaped String representation.
	 * 
	 * @param   terms    an array of Terms to convert
	 * @return  String representation of an array of Terms
	 */
	public static <T extends Term> String toEscapedString(T... terms) {
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < terms.length; ++i) {
			sb.append(terms[i].toEscapedString());
			if (i != terms.length - 1) {
				sb.append(", ");
			}
		}
		return sb.toString();
	}
	
	/**
	 * Converts a list of Terms to a String.
	 * 
	 * @param terms
	 * @return String representation of a list of Terms
	 */
	public static <T extends Term> String toEscapedString(List<T> terms) {
		return toEscapedString(terms.toArray(new Term[]{}));
	}

}
