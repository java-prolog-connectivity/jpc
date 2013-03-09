package org.jpc.util;

import static com.google.common.base.Preconditions.checkArgument;
import static java.util.Arrays.asList;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.jpc.JpcException;
import org.jpc.converter.TermConvertable;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.Term;
import org.jpc.term.Variable;

/**
 * An utility class for general purpose queries and term manipulation
 * Disclaimer: Some methods were inspired or taken from the JPL library
 * @author scastro
 *
 */
public class LogicUtil {
	
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
	
	/**
	 * Returns whether the resource name is an alias.
	 * Resource names can be either:
	 *   - an alias expressed as a compound term, like: 'library(my_module)' 
	 *   - or a source file located in the classpath, expressed as 'dir1/dir2/filename.ext' where the extension can be a prolog file (typically pl) or a Logtalk file (typically lgt) 
	 * @param resourceName the name of the resource
	 * @return whether the resource name is an alias.
	 */
	public static boolean isResourceAlias(String resourceName) {
		return resourceName.indexOf('(') != -1; //the current implementation just try a best guess based on the existence or not of an opening parenthesis
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
	
	
	public static String javaClassNameToProlog(String javaClassName) {
		String prologName = javaNameToProlog(javaClassName);
		String start = prologName.substring(0, 1);
		return start.toLowerCase() + prologName.substring(1);
	}
	
	public static String prologObjectNameToJava(String prologObjectName) {
		String javaName = prologNameToJava(prologObjectName);
		String start = javaName.substring(0, 1);
		return start.toUpperCase() + javaName.substring(1);
	}
	
	/*
	 * Transforms from camel case to prolog like names
	 */
	public static String javaNameToProlog(String javaName) {
		/*
		 * capital letters that do not have at the left:
		 * 	- another capital letter
		 *  - beginning of line
		 *  - an underscore
		 */
		Pattern pattern = Pattern.compile("[^^A-Z_][A-Z]");  
		Matcher matcher = pattern.matcher(javaName);
		
		StringBuffer sb = new StringBuffer();
		while (matcher.find()) {
			String matched = matcher.group();
			String replacement = matched.substring(0, 1) + "_" + matched.substring(1);
			matcher.appendReplacement(sb,replacement);
		}
		matcher.appendTail(sb);

		/*
		 * capital letters that have at the left:
		 * - another capital letter
		 * and have at the right:
		 *  - a non capital letter
		 */
		pattern = Pattern.compile("[A-Z][A-Z][a-z]");  
		matcher = pattern.matcher(sb.toString());
		
		sb = new StringBuffer();
		while (matcher.find()) {
			String matched = matcher.group();
			String replacement = matched.substring(0, 1) + "_" + matched.substring(1);
			matcher.appendReplacement(sb,replacement);
		}
		matcher.appendTail(sb);
		
		String start = sb.toString().substring(0,1);
		return start + sb.toString().substring(1).toLowerCase(); //will not modify the case of the first character
	}
	
	
	public static String prologNameToJava(String prologName) {
		Pattern pattern = Pattern.compile("_(\\w)");
		Matcher matcher = pattern.matcher(prologName);
		
		StringBuffer sb = new StringBuffer();
		while (matcher.find()) {
			String matched = matcher.group(1);
			String replacement = matched.toUpperCase();
			matcher.appendReplacement(sb,replacement);
		}
		matcher.appendTail(sb);
		return sb.toString();
	}
	
	
	public static String toString(Term term) {
		if(term instanceof IntegerTerm)
			return ""+((IntegerTerm)term).longValue();
		else if(term instanceof FloatTerm)
			return ""+((FloatTerm)term).doubleValue();
		else if(term instanceof Atom)
			return ((Atom)term).getName();
		else
			return term.toString();
	}
	
	public static int toInt(Term term) {
		return (int) toLong(term);
	}
	
	public static long toLong(Term term) {
		if(term instanceof IntegerTerm)
			return ((IntegerTerm)term).longValue();
		else if(term instanceof FloatTerm)
			return (long) toDouble(term);
		else if(term instanceof Atom)
			return Long.valueOf(((Atom)term).getName());
		else
			throw new JpcException("Impossible to convert the term " + term + " to a long");
	}
	
	public static float toFloat(Term term) {
		return (float) toDouble(term);
	}
	
	public static double toDouble(Term term) {
		if(term instanceof FloatTerm)
			return ((FloatTerm)term).doubleValue();
		else if(term instanceof Atom)
			return Double.valueOf(((Atom)term).getName());
		else
			throw new JpcException("Impossible to convert the term " + term + " to a double");
	}
	
	public static Number toNumber(Term term) {
		if(term instanceof IntegerTerm)
			return toLong(term);
		if(term instanceof FloatTerm)
			return toDouble(term);
		else if(term instanceof Atom)
			return Double.valueOf(((Atom)term).getName());
		else
			throw new JpcException("Impossible to convert the term " + term + " to a number");
	}
	
	public static List<Term> getChildren(Term term) {
		if(term.isList())
			return term.asList();
		else 
			return term.args();
	}

	/**
	 * Answers an array of anonymous logic variables
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
	
}
