package org.jpc.term;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;

import java.util.List;

import org.jpc.engine.visitor.AbstractJpcVisitor;
import org.jpc.engine.visitor.JpcDomVisitor;
import org.jpc.engine.visitor.JpcStreamingVisitor;

/**
 * A class reifying a logic compound term
 * DISCLAIMER: In the current version many methods in this class have been copied or adapted from the class jpl.Compound in the JPL library.
 * @author scastro
 *
 */
public class Compound extends Term {
	
	/**
	 * the name of this Compound
	 */
	protected final Term name;
	/**
	 * the arguments of this Compound
	 */
	protected final List<Term> args;
	
	/**
	 * Creates a Compound with name but no args (i.e. an Atom).
	 * This condsructor is protected (from illegal public use) and is used
	 * only by Atom, which inherits it.
	 * 
	 * @param   name   the name of this Compound
	 * @param   args   the arguments of this Compound
	 */
	/*
	protected Compound(String name) {
		checkNotNull(name);
		checkArgument(!name.isEmpty(), "The name of a logic variable cannot be an empty string");
		this.name = name;
		this.args = new ArrayList<>();
	}
	*/
	
	public <T extends TermAdaptable> Compound(String name, List<T> args) {
		this(new Atom(name), args);
	}
	
	/**
	 * Creates a Compound with name and args.
	 * 
	 * @param   name   the name of this Compound
	 * @param   args   the (one or more) arguments of this Compound
	 */
	public <T extends TermAdaptable> Compound(TermAdaptable name, List<T> args) {
		checkNotNull(name);
		checkNotNull(args);
		checkArgument(!args.isEmpty(), "A compound term must have at least one argument");
		this.name = name.asTerm();
		this.args = asTerms(args);
	}
	

	/**
	 * Tests whether this Compound's functor has (String) 'name' and 'arity'.
	 * 
	 * @return whether this Compound's functor has (String) 'name' and 'arity'
	 */
	
	//@Override
	public boolean hasFunctor(TermAdaptable nameTermObject, int arity) {
		return name.asTerm().termEquivalent(nameTermObject) && args.size() == arity;
	}
	
	
	
	/**
	 * Returns the name (unquoted) of this Compound.
	 * 
	 * @return the name (unquoted) of this Compound
	 */
	public Term name() {
		return name;
	}
	
	/**
	 * Returns the arguments of this Compound (1..arity) of this Compound as an array[0..arity-1] of Term.
	 * 
	 * @return the arguments (1..arity) of this Compound as an array[0..arity-1] of Term
	 */
	@Override
	public List<Term> args() {
		return args;
	}
	

	
	/**
	 * Returns a prefix functional representation of a Compound of the form name(arg1,...),
	 * and each argument is represented according to its toString() method.
	 * 
	 * @return  string representation of an Compound
	 */
	public String toString() {
		return name().toString() + (args.size() > 0 ? "(" + Term.toString(args) + ")" : "");
	}
	
	/**
	 * Two Compounds are equal if they are identical (same object) or their names and arities are equal and their
	 * respective arguments are equal.
	 * 
	 * @param   obj  the Object to compare (not necessarily another Compound)
	 * @return  true if the Object satisfies the above condition
	 */
	public boolean equals(Object obj) {
		return (this == obj || (obj instanceof Compound && name.equals(((Compound) obj).name) && equals(args, ((Compound) obj).args)));
	}
	
	public boolean termEquivalent(TermAdaptable termAdaptable) {
		Term term = termAdaptable.asTerm();
		if(term.isCompound()) {
			Compound compound = (Compound) term;
			return this.name.termEquivalent(compound.name) && termEquivalent(args, compound.args);
		}
		return false;
	}
	
	public void accept(JpcDomVisitor termVisitor) {
		if(termVisitor.visitCompound(this)) {
			name().accept(termVisitor);
			for(Term child: args) {
				child.accept(termVisitor);
			}
		}
	}


	public void accept(JpcStreamingVisitor termVisitor) {
		termVisitor.visitCompound();
		termVisitor.visitCompoundName();
		name().accept(termVisitor);
		termVisitor.endVisitCompoundName();
		for(Term child: args) {
			termVisitor.visitCompoundArg();
			child.accept(termVisitor);
			termVisitor.endVisitCompoundArg();
		}
		termVisitor.endVisitCompound();
	}


	public void accept(AbstractJpcVisitor termVisitor) {
		if(termVisitor instanceof JpcDomVisitor)
			accept((JpcDomVisitor)termVisitor);
		else if(termVisitor instanceof JpcStreamingVisitor)
			accept((JpcStreamingVisitor)termVisitor);
		else
			throw new RuntimeException("Unrecognized visitor: " + termVisitor.getClass().getName());
	}
}
