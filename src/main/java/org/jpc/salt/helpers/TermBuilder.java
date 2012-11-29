package org.jpc.salt.helpers;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * A utility class for creating term objects incrementally
 * @author sergioc
 *
 * @param <TermType> the term type this TermBuilder defines
 */
public abstract class TermBuilder<TermType> {

	public static <T>List<T> asTerms(Iterable<TermBuilder<T>> termBuilders) {
		return asTerms(termBuilders.iterator());
	}

	public static <T>List<T> asTerms(Iterator<TermBuilder<T>> termBuilders) {
		List<T> list = new ArrayList<>();
		while(termBuilders.hasNext()) {
			TermBuilder<T> tb = termBuilders.next();
			list.add(tb.asTerm());
		}
		return list;
	}
	
	
	private Object functor;//can be a TermType or a String
	private List<TermType> args;
	
	public TermBuilder() {
		setArgs(new ArrayList<TermType>());
	}
	
	public TermBuilder(Object functor) {
		this(functor, new ArrayList<TermType>());
	}
	
	public TermBuilder(Object functor, List<TermType> args) {
		setFunctor(functor);
		setArgs(args);
	}
	
	public abstract TermType asTerm();

	public boolean hasFunctor() {
		return functor != null;
	}
	
	public Object getFunctor() {
		return functor;
	}

	public void setFunctor(Object functor) {
		this.functor = functor;
	}

	public void addTerm() {
		
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

}
