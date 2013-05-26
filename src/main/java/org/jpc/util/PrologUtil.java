package org.jpc.util;

import static com.google.common.base.Preconditions.checkArgument;
import static java.util.Arrays.asList;
import static org.jpc.engine.prolog.PrologConstants.ANONYMOUS_VAR_NAME;

import java.util.ArrayList;
import java.util.List;

import org.jpc.salt.JpcTermWriter;
import org.jpc.salt.TermAdapter;
import org.jpc.salt.TermContentHandler;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.Term;
import org.jpc.term.Variable;

/**
 * An utility class for general purpose queries and term manipulation
 * Disclaimer: Some methods were inspired or taken from the JPL library
 * @author scastro
 *
 */
public class PrologUtil {
	
	/**
	 * Returns whether the term is a compound with functor("=", 2)
	 * @param term
	 * @return whether the term has as functor ("=", 2)
	 */
	public static boolean isEquals(Term term) {
		return term.hasFunctor("=", 2);
	}
	
	public static boolean isPair(Term term) {
		return term.hasFunctor("-", 2);
	}
	

	public static Term termSequence(Term... terms) {
		return termSequence(asList(terms));
	}
	
	public static Term termSequence(List<? extends Term> terms) {
		checkArgument(!terms.isEmpty());
		Term termSequence = terms.get(terms.size()-1);
		for(int i = terms.size()-2; i>=0; i--) {
			termSequence = new Compound(",", asList(terms.get(i), termSequence));
		}
		return termSequence;
	}
	
	public static List<Term> sequenceAsTerms(Term termSequence) {
		int len = sequenceLength(termSequence);
		Term[] ts = new Term[len];
		for (int i = 0; i < len; i++) {
			if(i<len-1) {
				ts[i] = termSequence.arg(1);
				termSequence = termSequence.arg(2);
			} else
				ts[i] = termSequence;
		}
		return asList(ts);
	} 

	public static int sequenceLength(Term termSequence) {
		int length = 1;
		if(termSequence instanceof Compound) {
			if(termSequence.hasFunctor(",", 2))
				length = 1 + sequenceLength(termSequence.arg(2));
		}
		return length;
	}
	
	/**
	 * Surround an atom with a functor
	 * @param atom
	 * @param functor
	 * @return
	 */
	public static Term applyFunctor(String functor, String atom) {
		return applyFunctor(functor, new Atom(atom));
	}
	
	private static Term applyFunctor(String functor, Term term) {
		return new Compound(functor, asList(term));
	}
	
	
	public static List<Term> forEachApplyFunctor(String functor, List<? extends Term> terms) {
		List<Term> appliedFunctorTerms = new ArrayList<>();
		for(Term term : terms) {
			appliedFunctorTerms.add(applyFunctor(functor, term));
		}
		return appliedFunctorTerms;
	}
	
	
	public static List<Term> getChildren(Term term) {
		if(term.isList())
			return term.asList();
		else 
			return term.getArgs();
	}

	/**
	 * Answers a list of anonymous logic variables
	 * @param n the number of variables in the array
	 * @return
	 */
	public static List<Variable> anonymousVariables(int n) {
		List<Variable> variablesList = new ArrayList<>();
		for(int i=0; i<n; i++) {
			variablesList.add(Variable.ANONYMOUS_VAR);
		}
		return variablesList;
	}
	

	public static final String RENAMED_VAR_PREFIX = "JPC_RENAMED_VAR_";

	public static Term replaceVariables(Term term) {
		return replaceVariables(term, RENAMED_VAR_PREFIX);
	}
	/**
	 * Renames the vars of a term adding a prefix
	 * Useful to avoid unexpected results when unifying two terms that do not share the same variable namespace
	 * @param term
	 * @return
	 */
	public static Term replaceVariables(Term term, final String prefix) {
		JpcTermWriter termWriter = new JpcTermWriter();
		TermAdapter variableAdapter = new TermAdapter(termWriter) {
			@Override
			public TermContentHandler startVariable(String name) {
				String newName = null;
				if(name.equals(ANONYMOUS_VAR_NAME)) {
					newName = ANONYMOUS_VAR_NAME;
				} else if(name.substring(0, 1).equals(ANONYMOUS_VAR_NAME)) {
					newName = ANONYMOUS_VAR_NAME + prefix + name;
				} else {
					newName = prefix + name;
				}
				super.startVariable(newName);
				return this;
			}
		};
		term.read(variableAdapter);
		return termWriter.getTerms().get(0);
	}
	
}
