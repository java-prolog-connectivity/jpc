package org.jpc.term.jterm;

import org.jpc.term.Compound;

public class JTermUtil {

	private static JTermManager defaultJTermManager;
	
	static {
		defaultJTermManager = new JTermManager(WeakReferencesCleaner.getWeakReferencesCleaner().getReferenceQueue());
		WeakReferencesCleaner.startWeakReferencesCleaner();
	}
	
	public static JTermManager getJTermManager() {
		return defaultJTermManager;
	}

	public static <T> JTerm<T> jTerm(Compound term, T o) {
		return (JTerm<T>) getJTermManager().jTerm(term, o);
	}
	
	public static <T> JTerm<T> jRef(T o) {
		JRefId refId = JRefIdManager.getJRefIdManager().getOrCreate(o);
		return (JTerm<T>)getJTermManager().jTerm(refId.asTerm(), o);
	}
	
	public static Compound jRefTerm(Object o) {
		return jRef(o).asTerm();
	}
}
