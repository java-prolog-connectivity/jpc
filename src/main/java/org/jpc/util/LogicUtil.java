package org.jpc.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.jpc.JpcException;
import org.jpc.engine.BootstrapLogicEngine;
import org.jpc.engine.LogicEngineConfiguration;
import org.jpc.flags.LogtalkFlag;
import org.jpc.term.Atom;
import org.jpc.term.Compound;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.Query;
import org.jpc.term.Term;
import org.jpc.term.Variable;

/**
 * An utility class for general purpose queries and term manipulation
 * DISCLAIMER: In the current version many methods in this class have been copied or adapted from the class jpl.Util in the JPL library.
 * @author scastro
 *
 */
public class LogicUtil {
	

	public static Term termsToList(List<Term> terms) {
		return termsToList(terms.toArray(new Term[]{}));
	}
	
	/**
	 * Converts an array of Terms to a Prolog list term
	 * whose members correspond to the respective array elements.
	 * 
	 * @param   terms  An array of Term
	 * @return  Term   a list of the array elements
	 */
	public static Term termsToList(Term... terms) {
		Term list = new Atom("[]");

		for (int i = terms.length - 1; i >= 0; --i) {
			list = new Compound(".", Arrays.asList(terms[i], list));
		}
		return list;
	}
	
	/**
	 * Converts an array of String to a corresponding Term list
	 * 
	 * @param a
	 *            An array of String objects
	 * @return Term a Term list corresponding to the given String array
	 */
	public static Term stringsToList(String... a) {
		Term list = new Atom("[]");
		for (int i = a.length - 1; i >= 0; i--) {
			list = new Compound(".", Arrays.asList(new Atom(a[i]), list));
		}
		return list;
	}
	
	/**
	 * Converts an array of int to a corresponding term list
	 * 
	 * @param a
	 *            An array of int values
	 * @return a term list corresponding to the given int array
	 */
	public static Term intsToList(int... a) {
		Term list = new Atom("[]");
		for (int i = a.length - 1; i >= 0; i--) {
			list = new Compound(".", Arrays.asList(new IntegerTerm(a[i]), list));
		}
		return list;
	}
	
	/**
	 * Converts an array of arrays of int to a corresponding list of lists
	 * 
	 * @param a
	 *            An array of arrays of int values
	 * @return a term list of lists corresponding to the given int array of arrays
	 */
	public static Term intTableToList(int[][] a) {
		Term list = new Atom("[]");
		for (int i = a.length - 1; i >= 0; i--) {
			list = new Compound(".", Arrays.asList(intsToList(a[i]), list));
		}
		return list;
	}
	
	
	public static int listToLength(Term t) {
		int length = 0;
		Term head = t;
		while (head.hasFunctor(".", 2)) {
			length++;
			head = head.arg(2);
		}
		return (head.hasFunctor("[]", 0) ? length : -1);
	}
	
	/** converts a proper list to an array of terms, else throws an exception
	 * 
	 * @throws LException
	 * @return an array of terms whose successive elements are the corresponding members of the list (if it is a list)
	 */
	public static List<Term> listToTerms(Term t) {
		try {
			int len = t.listLength();
			Term[] ts = new Term[len];

			for (int i = 0; i < len; i++) {
				ts[i] = t.arg(1);
				t = t.arg(2);
			}
			return Arrays.asList(ts);
		} catch (Exception e) {
			throw new JpcException("term " + t + " is not a proper list");
		}
	}
	
	public static List<String> atomListToStrings(Term t){
		int n = listToLength(t);
		String[] a = new String[n];
		int i = 0;
		Term head = t;
		while ( head.hasFunctor(".", 2)){
			Term x = head.arg(1);
			if ( x.isAtom()){
				a[i++]=((Atom)x).name();
			} else {
				return null;
			}
			head = head.arg(2);
		}
		return (head.hasFunctor("[]", 0) ? Arrays.asList(a) : null );
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
	
	public static Term applyFunctor(String functor, Term term) {
		return new Compound(functor, Arrays.asList(term));
	}
	
	public static List<Term> forAllApplyFunctor(String functor, List<Term> terms) {
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
		if(term.isInteger())
			return ""+((IntegerTerm)term).longValue();
		else if(term.isFloat())
			return ""+((FloatTerm)term).doubleValue();
		else if(term.isAtom())
			return ((Atom)term).name();
		else
			return term.toString();
	}
	
	public static int toInt(Term term) {
		return (int) toLong(term);
	}
	
	public static long toLong(Term term) {
		if(term.isInteger())
			return ((IntegerTerm)term).longValue();
		else if(term.isFloat())
			return (long) toDouble(term);
		else if(term.isAtom())
			return Long.valueOf(((Atom)term).name());
		else
			throw new JpcException("Impossible to convert the term " + term + " to a long");
	}
	
	public static float toFloat(Term term) {
		return (float) toDouble(term);
	}
	
	public static double toDouble(Term term) {
		if(term.isFloat())
			return ((FloatTerm)term).doubleValue();
		else if(term.isAtom())
			return Double.valueOf(((Atom)term).name());
		else
			throw new JpcException("Impossible to convert the term " + term + " to a double");
	}
	
	public static Number toNumber(Term term) {
		if(term.isInteger())
			return toLong(term);
		if(term.isFloat())
			return toDouble(term);
		else if(term.isAtom())
			return Double.valueOf(((Atom)term).name());
		else
			throw new JpcException("Impossible to convert the term " + term + " to a number");
	}
	
	public static List<Term> getChildren(Term term) {
		if(term.isCompound()) {
			if(term.isList())
				return listToTerms(term);
			else
				return term.args();
		} else
			return Collections.emptyList();
	}

}
