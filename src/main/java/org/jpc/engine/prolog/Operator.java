package org.jpc.engine.prolog;

import java.util.ArrayList;
import java.util.List;

import org.jpc.converter.TermConvertable;
import org.jpc.term.Atom;
import org.jpc.term.IntegerTerm;
import org.jpc.term.ListTerm;
import org.jpc.term.Term;

/**
 * This class is experimental and may be deleted.
 * @author sergioc
 *
 */
public class Operator implements TermConvertable<Atom> {

	private String name;
	private Specifier specifier;
	private int priority;
	
	public Operator(String name, Specifier specifier, int priority) {
		this.name = name;
		this.specifier = specifier;
		this.priority = priority;
	}

	public String getName() {
		return name;
	}
	
	public Specifier getSpecifier() {
		return specifier;
	}
	
	public int getPriority() {
		return priority;
	}

	@Override
	public Atom asTerm() {
		return new Atom(name);
	}

	public static List<Operator> asOperators(Iterable<? extends Term> operatorsListTerm) {
		List<Operator> operators = new ArrayList<>();
		for(Term operatorTerm : operatorsListTerm) {
			operators.add(asOperator(operatorTerm));
		}
		return operators;
	}
	
	/**
	 * Constructs an operator from a term having as structure [P,S,N]
	 * Where P is the operator's priority, S is its specifier (an integer term) and N the operator's name
	 * @param operatorTerm the term representing an operator
	 * @return an operator
	 */
	public static Operator asOperator(Term operatorTerm) {
		ListTerm operatorItems = operatorTerm.asList();
		int priority = ((IntegerTerm)operatorItems.get(0)).intValue();
		Specifier specifier = Specifier.valueOf(((Atom)operatorItems.get(1)).getName());
		String name = ((Atom)operatorItems.get(2)).getName();
		Operator op = new Operator(name, specifier, priority);
		return op;
	}
	
}
