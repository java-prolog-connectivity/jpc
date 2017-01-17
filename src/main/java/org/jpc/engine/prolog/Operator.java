package org.jpc.engine.prolog;


import org.jpc.converter.TermConvertable;
import org.jpc.term.Atom;
import org.jpc.term.Integer;
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

	/**
	 * Constructs an operator from a term having as structure [P,S,N]
	 * Where P is the operator's priority, S is its specifier (an integer term) and N the operator's id
	 * @param operatorTerm the term representing an operator
	 * @return an operator
	 */
	public static Operator asOperator(Term operatorTerm) {
		ListTerm operatorItems = operatorTerm.asList();
		int priority = ((Integer)operatorItems.get(0)).intValue();
		Specifier specifier = Specifier.valueOf(((Atom)operatorItems.get(1)).getName());
		String name = ((Atom)operatorItems.get(2)).getName();
		Operator op = new Operator(name, specifier, priority);
		return op;
	}
	
	public boolean isUnary() {
		return specifier.isUnary();
	}
	
	public boolean isBinary() {
		return specifier.isBinary();
	}
	
	public boolean isPrefix() {
		return specifier.isPrefix();
	}
	
	public boolean isPostfix() {
		return specifier.isPostfix();
	}
	
	public boolean isInfix() {
		return specifier.isInfix();
	}

}
