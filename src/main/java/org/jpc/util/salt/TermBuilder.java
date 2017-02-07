package org.jpc.util.salt;

import java.util.ArrayList;
import java.util.List;

/**
 * A utility class for creating term objects incrementally
 * @author sergioc
 *
 * @param <TermType> the term type this TermBuilder defines
 */
public abstract class TermBuilder<TermType> {
	
	private TermType functor;//can be a TermType or a String
	private List<TermType> args;
	
	public TermBuilder() {
		setArgs(new ArrayList<TermType>());
	}
	
	public TermBuilder(TermType functor) {
		this(functor, new ArrayList<TermType>());
	}
	
	public TermBuilder(TermType functor, List<TermType> args) {
		setFunctor(functor);
		setArgs(args);
	}
	
	public abstract TermType build();

	public boolean hasFunctor() {
		return functor != null;
	}
	
	public TermType getFunctor() {
		return functor;
	}

	public void setFunctor(TermType functor) {
		this.functor = functor;
	}
	
	public List<TermType> getArgs() {
		return args;
	}

	private void setArgs(List<TermType> args) {
		this.args = args;
	}
	
	public void addArg(TermType arg) {
		this.args.add(arg);
	}
	
	public int arity() {
		return args.size();
	}
	
	public boolean isCompound() {
		return arity() > 0;
	}

}
