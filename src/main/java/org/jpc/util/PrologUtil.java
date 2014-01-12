package org.jpc.util;

import static java.util.Arrays.asList;
import static org.jpc.engine.prolog.PrologConstants.ANONYMOUS_VAR_NAME;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.regex.Matcher;

import org.jpc.salt.JpcTermWriter;
import org.jpc.salt.TermAdapter;
import org.jpc.salt.TermContentHandler;
import org.jpc.term.AbstractVar;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;
import org.jpc.term.Var;

/**
 * An utility class for general purpose queries and term manipulation
 * Disclaimer: Some methods were inspired or adapted from the JPL library
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
	
	public static boolean isSequence(Term term) {
		return term.hasFunctor(",", 2);
	}
	
	public static Term termSequence(Term... terms) {
		return termSequence(asList(terms));
	}
	
	public static Term termSequence(Collection<Term> terms) {
		return new ListTerm(terms).asSequence();
	}
	
	public static Term varDictionaryTerm(Term term) {
		ListTerm mapVarsNames = new ListTerm();
		for(AbstractVar var : term.getNamedVariables()) {
			Compound varNameEntry = new Compound("=", asList(new Atom(var.getName()), var));
			mapVarsNames.add(varNameEntry);
		}
		Term mapVarsNamesTerm = mapVarsNames.asTerm();
		return mapVarsNamesTerm;
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
	public static List<Var> anonymousVariables(int n) {
		List<Var> variablesList = new ArrayList<>();
		for(int i=0; i<n; i++) {
			variablesList.add(Var.ANONYMOUS_VAR);
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
	
	public static String escapeString(String s) {
		String escapedString = s;
		escapedString = escapedString.replaceAll("\\\\", Matcher.quoteReplacement("\\\\"));
		escapedString = escapedString.replaceAll("'", Matcher.quoteReplacement("''")); //escaping ' with \' does not work correctly in XSB, therefore it is escaped with the alternative ''
		escapedString = "'" + escapedString + "'";
		return escapedString;
	}
	
}
