package org.jpc.term;

import static com.google.common.base.Preconditions.checkArgument;
import static org.jpc.engine.prolog.PrologConstants.CONS_FUNCTOR;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.jpc.JpcException;
import org.jpc.engine.prolog.Operator;
import org.jpc.engine.prolog.OperatorsContext;
import org.jpc.salt.TermContentHandler;
import org.jpc.term.visitor.TermVisitor;

import com.google.common.base.Joiner;

/**
 * A class reifying a logic compound term
 * @author scastro
 *
 */
public final class Compound extends AbstractTerm {

	/**
	 * the name of this Compound
	 */
	private final Term name;
	/**
	 * the arguments of this Compound
	 */
	private final List<Term> args;

	
	public Compound(String name, List<? extends Term> args) {
		this(new Atom(name), args);
	}
	
	/**
	 * Creates a Compound with name and args.
	 * 
	 * @param   name   the name of this Compound
	 * @param   args   the (one or more) arguments of this Compound
	 */
	public Compound(Term name, List<? extends Term> args) {
		checkArgument(!args.isEmpty(), "A compound term must have at least one argument");
		this.name = name;
		this.args = (List<Term>) args;
	}
	
	@Override
	public boolean isHilog() {
		return !(name instanceof Atom);
	}
	
	@Override
	public boolean isList() {
		return isCons() && arg(2).isList();
	}
	
	public boolean isCons() {
		return hasFunctor(CONS_FUNCTOR, 2);
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
	
	public boolean hasName(String name) {
		return hasName(new Atom(name));
	}
	
	public boolean hasName(Term term) {
		return name.termEquals(term);
	}

	/**
	 * Tests whether this Compound's functor has (String) 'name' and 'arity'.
	 * 
	 * @return whether this Compound's functor has (String) 'name' and 'arity'
	 */
	
	//@Override
	public boolean hasFunctor(Term nameTermObject, int arity) {
		return name.termEquals(nameTermObject) && args.size() == arity;
	}
	
	
	/**
	 * Returns the name of this Compound.
	 * 
	 * @return the name of this Compound
	 */
	public Term getName() {
		return name;
	}
	
//	public String getNameString() {
//		if(name instanceof Atom) {
//			return ((Atom)name).getName();
//		} else {
//			throw new RuntimeException("The compound functor is not an atom: " + name);
//		}
//	}
	
	/**
	 * Returns the arguments of this Compound (1..arity) of this Compound as a List of Term.
	 * 
	 * @return the arguments of this Compound (1..arity) of this Compound as a List of Term.
	 */
	@Override
	public List<Term> getArgs() {
		return args;
	}
	
	
	/**
	 * Returns a prefix functional representation of a Compound of the form name(arg1,...),
	 * 
	 * @return  string representation of an Compound
	 */
	@Override
	public String toEscapedString() {
		return getName().toEscapedString() + "(" + AbstractTerm.toEscapedString(args) + ")";
	}
	
	@Override
	public int hashCode() {
		List<Term> allFields = new ArrayList<Term>(getArgs());
		allFields.add(0, getName());
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
	public boolean termEquals(Term term) {
		if(term instanceof Compound) {
			Compound compound = (Compound) term;
			return this.name.termEquals(compound.name) && termEquals(args, compound.args);
		}
		return false;
	}
	
	public void accept(TermVisitor termVisitor) {
		if(termVisitor.visitCompound(this)) {
			getName().accept(termVisitor);
			for(Term child: args) {
				child.accept(termVisitor);
			}
		}
	}

	public boolean usesOperator(Operator operator) {
		return(hasName(operator.getName()) &&
			((operator.isUnary() && arity() == 1) || 
			(operator.isBinary() && arity() == 2)));
	}
	
	@Override
	public void basicRead(TermContentHandler contentHandler, Map<Term,Term> replacements) {
		contentHandler.startCompound();
		getName().read(contentHandler, replacements);
		for(Term child: args) {
			child.read(contentHandler, replacements);
		}
		contentHandler.endCompound();
	}

	@Override
	public String toString(OperatorsContext operatorsContext) {
		StringBuilder sb = new StringBuilder();
		if(isList()) {
			sb.append("[");
			List<String> members = new ArrayList<>();
			for(Term term : asList()) {
				members.add(term.toString(operatorsContext));
			}
			sb.append(Joiner.on(",").join(members));
			sb.append("]");
		} else {
			Operator op = operatorsContext.getOperator(this);
			if(op != null) {
				if(op.isUnary()) {
					if(op.isPrefix()) {
						sb.append(op.getName());
						sb.append(arg(1).toString(operatorsContext));
					} else {
						sb.append(arg(1).toString(operatorsContext));
						sb.append(op.getName());
					}
				} else {
					sb.append(arg(1).toString(operatorsContext));
					sb.append(op.getName());
					sb.append(arg(2).toString(operatorsContext));
				}
					
			} else
				sb.append(toString());
		}
		return sb.toString();
	}

}
