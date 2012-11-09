package org.jpc.term;

import static com.google.common.base.Preconditions.checkArgument;
import static org.jpc.util.DefaultTermAdapter.asTerms;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import org.jpc.engine.visitor.AbstractJplVisitor;
import org.jpc.engine.visitor.JpcDomVisitor;
import org.jpc.engine.visitor.JpcStreamingVisitor;

/**
 * A class reifying a logic compound term
 * @author scastro
 *
 */
public class Compound extends AbstractTerm {
	
	/**
	 * the name of this Compound
	 */
	protected final Term name;
	/**
	 * the arguments of this Compound
	 */
	protected final List<Term> args;

	
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
		return name.asTerm().termEquals(nameTermObject) && args.size() == arity;
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
	@Override
	public String toString() {
		return name().toString() + (args.size() > 0 ? "(" + AbstractTerm.toString(args) + ")" : "");
	}
	
	@Override
	public int hashCode() {
		List<Term> allFields = new ArrayList<Term>(args());
		allFields.add(0, name());
		return Objects.hash(allFields.toArray());
	}
	
	/**
	 * Two Compounds are equal if they are identical (same object) or their names and arities are equal and their
	 * respective arguments are equal.
	 * 
	 * @param   obj  the Object to compare (not necessarily another Compound)
	 * @return  true if the Object satisfies the above condition
	 */
	@Override
	public boolean equals(Object obj) {
		return (this == obj || (obj instanceof Compound && 
				((Compound) obj).canEquals(this) &&
				name.equals(((Compound) obj).name) && 
				Arrays.equals(args.toArray(), ((Compound) obj).args.toArray())));
	}
	
	/**
	 * Any class overriding equals should override this method
	 * @param obj the object to compare
	 * @return whether this instance can equals the object sent as parameter
	 */
	public boolean canEquals(Object obj) {
		return obj instanceof Compound;
	}
	
	@Override
	public boolean termEquals(TermAdaptable termAdaptable) {
		Term term = termAdaptable.asTerm();
		if(term.isCompound()) {
			Compound compound = (Compound) term;
			return this.name.termEquals(compound.name) && termEquals(args, compound.args);
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


	public void accept(AbstractJplVisitor termVisitor) {
		if(termVisitor instanceof JpcDomVisitor)
			accept((JpcDomVisitor)termVisitor);
		else if(termVisitor instanceof JpcStreamingVisitor)
			accept((JpcStreamingVisitor)termVisitor);
		else
			throw new RuntimeException("Unrecognized visitor: " + termVisitor.getClass().getName());
	}
}
