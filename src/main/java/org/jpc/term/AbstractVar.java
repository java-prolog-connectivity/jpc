package org.jpc.term;

import java.util.ArrayList;
import java.util.List;

import org.jpc.engine.prolog.OperatorsContext;
import org.jpc.util.salt.TermContentHandler;
import org.jpc.term.visitor.TermVisitor;

import com.google.common.base.Function;

/**
 * An abstract Prolog variable.
 * @author sergioc
 *
 */
public abstract class AbstractVar extends Term {
	
	public abstract boolean isAnonymous();
	
	public abstract String getName();
	
	@Override
	public boolean isGround() {
		return false;
	}
	
	/**
	 * @return the variables names present in the term.
	 */
	public static List<String> getVariableNames(List<AbstractVar> vars) {
		List<String> names = new ArrayList<>();
		for(AbstractVar var : vars) {
			names.add(var.getName());
		}
		return names;
	}
	
	@Override
	public boolean hasFunctor(Functor functor) {
		return functor.getArity() == 0 && termEquals(functor.getName());
	}

	@Override
	public void accept(TermVisitor termVisitor) {
		termVisitor.visitVariable(this);
	}

	@Override
	protected void basicRead(TermContentHandler contentHandler, Function<Term, Term> termExpander) {
		contentHandler.startVariable(getName());	
	}

	
	@Override
	public String toString(OperatorsContext operatorsContext) {
		return toString();
	}
	
	/**
	 * Returns a Prolog source text representation of this Variable
	 * 
	 * @return  a Prolog source text representation of this Variable
	 */
	@Override
	public String toString() {
		return getName();
	}
	
	@Override
	public String toEscapedString() {
		return toString();
	}
	
	/**
	 * A Variable is equal to another if their names are the same and they are not anonymous.
	 * 
	 * @param   obj  The Object to compare.
	 * @return  true if the Object is a Variable and the above condition apply.
	 */
	@Override
	public boolean equals(Object obj) {
		try {
			return !isAnonymous() && termEquals((Term) obj);
		} catch(ClassCastException e) {
			return false;
		}
	}

}
