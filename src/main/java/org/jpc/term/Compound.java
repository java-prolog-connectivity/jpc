package org.jpc.term;

import static com.google.common.base.Preconditions.checkArgument;
import static org.jpc.util.DefaultTermConverter.asTermList;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import org.jpc.JpcException;
import org.jpc.salt.ContentHandler;
import org.jpc.visitor.JpcVisitor;

/**
 * A class reifying a logic compound term
 * @author scastro
 *
 */
public final class Compound extends AbstractTerm {
	
	public boolean isUnification() {
		return hasFunctor("=", 2);
	}
	
	/**
	 * the name of this Compound
	 */
	protected final Term name;
	/**
	 * the arguments of this Compound
	 */
	protected final List<Term> args;

	
	public <T extends TermConvertable> Compound(String name, List<T> args) {
		this(new Atom(name), args);
	}
	
	/**
	 * Creates a Compound with name and args.
	 * 
	 * @param   name   the name of this Compound
	 * @param   args   the (one or more) arguments of this Compound
	 */
	public <T extends TermConvertable> Compound(TermConvertable name, List<T> args) {
		checkArgument(!args.isEmpty(), "A compound term must have at least one argument");
		this.name = name.asTerm();
		this.args = asTermList(args);
	}
	
	
	@Override
	public boolean isList() {
		return isCons() && arg(2).isList();
	}
	
	public boolean isCons() {
		return hasFunctor(".", 2);
	}
	
	@Override
	public ListTerm asList() {
		if(isList()) {
			ListTerm list = new ListTerm();
			Term current = this;
			while(!current.equals(Atom.EMPTY_LIST)) {
				list.add(current.arg(1));
				current = current.arg(2);
			}
			return list;
		} else
			throw new JpcException("The term " + this + " is not a list");
	}
	

	/**
	 * Tests whether this Compound's functor has (String) 'name' and 'arity'.
	 * 
	 * @return whether this Compound's functor has (String) 'name' and 'arity'
	 */
	
	//@Override
	public boolean hasFunctor(TermConvertable nameTermObject, int arity) {
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
				name.equals(((Compound) obj).name) && 
				Arrays.equals(args.toArray(), ((Compound) obj).args.toArray())));
	}
	
	@Override
	public boolean termEquals(TermConvertable termConvertable) {
		Term term = termConvertable.asTerm();
		if(term instanceof Compound) {
			Compound compound = (Compound) term;
			return this.name.termEquals(compound.name) && termEquals(args, compound.args);
		}
		return false;
	}
	
	public void accept(JpcVisitor termVisitor) {
		if(termVisitor.visitCompound(this)) {
			name().accept(termVisitor);
			for(Term child: args) {
				child.accept(termVisitor);
			}
		}
	}

	@Override
	public void streamTo(ContentHandler contentHandler) {
		contentHandler.startCompound();
		contentHandler.startCompoundName();
		name().streamTo(contentHandler);
		contentHandler.endCompoundName();
		for(Term child: args) {
			contentHandler.startCompoundArg();
			child.streamTo(contentHandler);
			contentHandler.endCompoundArg();
		}
		contentHandler.endCompound();
	}

}
