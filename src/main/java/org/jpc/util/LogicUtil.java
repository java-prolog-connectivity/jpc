package org.jpc.util;

import static com.google.common.base.Preconditions.checkArgument;
import static java.util.Arrays.asList;

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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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

/**
 * An utility class for general purpose queries and term manipulation
 * Disclaimer: Some methods were inspired or taken from the JPL library
 * @author scastro
 *
 */
public class LogicUtil {
	
	/**
	 * Returns whether the term is an unification
	 * A term is an unification if it is a compound with functor("=", 2)
	 * @param term
	 * @return whether the term is an unification
	 */
	public static boolean isUnification(TermConvertable termConvertable) {
		Term term = termConvertable.asTerm();
		return term instanceof Compound && ((Compound)term).isUnification();
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
	

	public static Term termsToSequence(List<? extends TermConvertable> termConvertables) {
		checkArgument(!termConvertables.isEmpty());
		Term termSequence = termConvertables.get(termConvertables.size()-1).asTerm();
		for(int i = termConvertables.size()-2; i>=0; i--) {
			termSequence = new Compound(",", asList(termConvertables.get(i), termSequence));
		}
		return termSequence;
	}
	
	public static List<Term> sequenceAsTerms(TermConvertable termConvertableSequence) {
		Term termSequence = termConvertableSequence.asTerm();
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

	public static int sequenceLength(TermConvertable termConvertableSequence) {
		Term termSequence = termConvertableSequence.asTerm();
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
	
	private static Term applyFunctor(String functor, TermConvertable termConvertable) {
		return new Compound(functor, asList(termConvertable));
	}
	
	
	public static List<Term> forEachApplyFunctor(String functor, List<? extends TermConvertable> termConvertables) {
		List<Term> appliedFunctorTerms = new ArrayList<>();
		for(TermConvertable termConvertable : termConvertables) {
			appliedFunctorTerms.add(applyFunctor(functor, termConvertable));
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
	
	
	public static String toString(TermConvertable termConvertable) {
		Term term = termConvertable.asTerm();
		if(term instanceof IntegerTerm)
			return ""+((IntegerTerm)term).longValue();
		else if(term instanceof FloatTerm)
			return ""+((FloatTerm)term).doubleValue();
		else if(term instanceof Atom)
			return ((Atom)term).name();
		else
			return term.toString();
	}
	
	public static int toInt(TermConvertable termConvertable) {
		return (int) toLong(termConvertable);
	}
	
	public static long toLong(TermConvertable termConvertable) {
		Term term = termConvertable.asTerm();
		if(term instanceof IntegerTerm)
			return ((IntegerTerm)term).longValue();
		else if(term instanceof FloatTerm)
			return (long) toDouble(term);
		else if(term instanceof Atom)
			return Long.valueOf(((Atom)term).name());
		else
			throw new JpcException("Impossible to convert the term " + term + " to a long");
	}
	
	public static float toFloat(TermConvertable termConvertable) {
		return (float) toDouble(termConvertable);
	}
	
	public static double toDouble(TermConvertable termConvertable) {
		Term term = termConvertable.asTerm();
		if(term instanceof FloatTerm)
			return ((FloatTerm)term).doubleValue();
		else if(term instanceof Atom)
			return Double.valueOf(((Atom)term).name());
		else
			throw new JpcException("Impossible to convert the term " + term + " to a double");
	}
	
	public static Number toNumber(TermConvertable termConvertable) {
		Term term = termConvertable.asTerm();
		if(term instanceof IntegerTerm)
			return toLong(term);
		if(term instanceof FloatTerm)
			return toDouble(term);
		else if(term instanceof Atom)
			return Double.valueOf(((Atom)term).name());
		else
			throw new JpcException("Impossible to convert the term " + term + " to a number");
	}
	
	public static List<Term> getChildren(TermConvertable termConvertable) {
		Term term = termConvertable.asTerm();
		if(term.isList())
			return term.asList();
		else 
			return term.args();
	}

	
	
}
