package org.jpc.term;

import static com.google.common.base.Preconditions.checkArgument;
import static org.jpc.engine.prolog.PrologConstants.CONS_FUNCTOR;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import org.jpc.JpcException;
import org.jpc.engine.prolog.Operator;
import org.jpc.engine.prolog.OperatorsContext;
import org.jpc.salt.TermContentHandler;
import org.jpc.term.compiler.CompilationContext;
import org.jpc.term.compiler.Environment;
import org.jpc.term.unification.NonUnifiableException;
import org.jpc.term.visitor.TermVisitor;

import com.google.common.base.Function;
import com.google.common.base.Joiner;

/**
 * A class reifying a logic compound term
 * @author scastro
 *
 */
public final class Compound extends Term {

	private Boolean ground;
	private Integer hash;
	/**
	 * the id of this Compound
	 */
	private final Term name;
	
	/**
	 * the arguments of this Compound
	 */
	private final List<Term> args;

	private final Functor functor;
	
	public Compound(String name, List<? extends Term> args) {
		this(new Atom(name), args);
	}
	
	/**
	 * Creates a Compound with id and args.
	 * 
	 * @param   id   the id of this Compound
	 * @param   args   the (one or more) arguments of this Compound
	 */
	public Compound(Term name, List<? extends Term> args) {
		checkArgument(!args.isEmpty(), "A compound term must have at least one argument");
		this.name = name;
		this.args = (List<Term>) args;
		functor = new Functor(name, arity());
	}
	
	@Override
	public boolean isHilog() {
		return !(name instanceof Atom);
	}
	
	@Override
	protected boolean basicIsList() {
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
			while(!current.equals(Atom.NIL)) {
				list.add(current.arg(1));
				current = current.arg(2);
			}
			return list;
		} else
			throw new JpcException("The term " + this + " is not a list");
	}
	
	public boolean hasName(String name) {
		return getNameString().equals(name);
	}
	
	public boolean hasName(Term term) {
		return name.termEquals(term);
	}


	@Override
	public boolean hasFunctor(Functor functor) {
		return args.size() == functor.getArity() && name.termEquals(functor.getName());
	}
	
	public Functor getFunctor() {
		return functor;
	}
	
	/**
	 * Returns the id of this Compound.
	 * 
	 * @return the id of this Compound
	 */
	public Term getName() {
		return name;
	}
	
	public String getNameString() {
		if(name instanceof Atom) {
			return ((Atom)name).getName();
		} else {
			throw new RuntimeException("The compound functor is not an atom: " + name);
		}
	}
	
	/**
	 * Returns the arguments of this Compound (1..arity) of this Compound as a List of Term.
	 * 
	 * @return the arguments of this Compound (1..arity) of this Compound as a List of Term.
	 */
	@Override
	public List<Term> getArgs() {
		return args;
	}

	@Override
	public boolean isGround() {
		if(ground == null) {
			boolean tmpGround = true;
			if(!getName().isGround())
				tmpGround = false;
			else {
				for(Term arg : getArgs()) {
					if(!arg.isGround()) {
						tmpGround = false;
						break;
					}
				}
			}
			ground = tmpGround;
		}
		return ground;
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
	protected void basicRead(TermContentHandler contentHandler, Function<Term, Term> termExpander) {
		contentHandler.startCompound();
		getName().read(contentHandler, termExpander);
		for(Term child: args) {
			child.read(contentHandler, termExpander);
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
	
	/**
	 * Returns a prefix functional representation of a Compound of the form id(arg1,...),
	 * 
	 * @return  string representation of an Compound
	 */
	@Override
	public String toEscapedString() {
		return getName().toEscapedString() + "(" + Term.toEscapedString(args) + ")";
	}
	
	@Override
	public final int hashCode() {
		if(hash == null)
			hash = basicHashCode();
		return hash;
	}

	private int basicHashCode() {
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
		return (this == obj || (obj.getClass().equals(getClass()) && 
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

	
	@Override
	public void unify(Term term) {
		if(this != term) {
			if(term instanceof AbstractVar || term instanceof JRef)
				term.unify(this);
			else if(!(term instanceof Compound) || term.arity() != arity())
				throw new NonUnifiableException(this, term);
			else {
				Compound compound = (Compound) term;
				getName().unify(compound.getName());
				for(int i=0; i<arity(); i++)
					arg(i+1).unify(term.arg(i+1));
			}
		}
	}
	
	@Override
	public Term preCompile(Environment env, CompilationContext context) {
		Compound compiledCompound;
		Term compiledName = getName().preCompile(env, context);
		List<Term> compiledArgs = new ArrayList<>();
		for(Term arg : getArgs()) {
			compiledArgs.add(arg.preCompile(env, context));
		}
		compiledCompound = new Compound(compiledName, compiledArgs);
		compiledCompound.ground = isGround();
		return compiledCompound;
	}

	@Override
	public Term prepareForQuery(CompilationContext context) {
		Compound compiledCompound;
		Term compiledName = getName().prepareForQuery(context);
		List<Term> compiledArgs = new ArrayList<>();
		for(Term arg : getArgs()) {
			compiledArgs.add(arg.prepareForQuery(context));
		}
		compiledCompound = new Compound(compiledName, compiledArgs);
		compiledCompound.ground = isGround();
		return compiledCompound;
	}
	
	@Override
	public Term prepareForFrame(CompilationContext context) {
		Compound framedCompound;
		if(isGround())
			framedCompound = this;
		else {
			Term framedName = getName();
			if(!framedName.isGround())
				framedName = framedName.prepareForFrame(context);
			List<Term> framedArgs = new ArrayList<>();
			for(Term arg : getArgs()) {
				if(!arg.isGround())
					arg = arg.prepareForFrame(context);
				framedArgs.add(arg);
			}
			framedCompound = new Compound(framedName, framedArgs);
			framedCompound.ground = isGround();
		}
		return framedCompound;
	}

}
