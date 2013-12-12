package org.jpc.term.jterm;

import org.jpc.term.Compound;

import com.google.common.base.Function;

/**
 * Utility methods to facilitate the creation of terms representing object references.
 * @author sergioc
 *
 */
public class JTermUtil {

	private static JTermManager defaultJTermManager;
	
	static {
		defaultJTermManager = new JTermManager(WeakReferencesCleaner.getWeakReferencesCleaner().getReferenceQueue());
		WeakReferencesCleaner.startWeakReferencesCleaner();
	}
	
	/**
	 * @return the default JTermManager
	 */
	public static JTermManager getJTermManager() {
		return defaultJTermManager;
	}

	/**
	 * Maps an object to a given term representation.
	 * @param term the term representation of the object sent as second argument.
	 * @param o the object to express as a JTerm reference associated with the term sent as first argument.
	 * @return a JTerm reference on the object sent as second argument, uniquely identified by the term sent as first argument.
	 */
	public static <T> JTerm<T> jTerm(Compound term, T o) {
		return getJTermManager().jTerm(term, o);
	}
	
	/**
	 * Maps an object to a compound term of the form jref(id). id is an arbitrary integer uniquely identifying an object reference.
	 * @param o the object to express as a JTerm reference.
	 * @return a JTerm reference on the object sent as argument.
	 */
	public static <T> JTerm<T> jRef(T o) {
		JRefId refId = JRefIdManager.getJRefIdManager().getOrCreate(o);
		return jTerm(refId.asTerm(), o);
	}
	
	/**
	 * 
	 * @param o the object to express as a term reference.
	 * @return the term representation of a reference.
	 */
	public static Compound jRefTerm(Object o) {
		return jRef(o).asTerm();
	}
	
	/**
	 * Registers an index function with a compound id.
	 * @param id the compound id associated with an index function.
	 * @param indexFunction the index function.
	 * @throws RuntimeException if an index associated with the given id already exists.
	 */
	public static void addIndexFunction(String name, Function<Compound, Object> indexFunction) {
		getJTermManager().addIndexFunction(name, indexFunction);
	}
}
