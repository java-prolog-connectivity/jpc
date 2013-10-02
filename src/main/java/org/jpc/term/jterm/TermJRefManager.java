package org.jpc.term.jterm;

import java.lang.ref.ReferenceQueue;

import org.jpc.term.Term;

public class TermJRefManager extends IdentifiableWeakReferenceManager<Term, Object> {

	private static TermJRefManager defaultTermJRefManager;
	
	static {
		defaultTermJRefManager = new TermJRefManager(WeakReferencesCleaner.getDefaultReferenceQueue());
		WeakReferencesCleaner.startWeakReferencesCleaner();
	}
	
	public static TermJRefManager getTermJRefManager() {
		return defaultTermJRefManager;
	}
	
	public static <T> TermJRef<T> save(Term termId, T o) {
		return (TermJRef<T>) defaultTermJRefManager.weakReference(termId, o);
	}
	
	public TermJRefManager(ReferenceQueue<?> referenceQueue){
		super(referenceQueue);
	}

	protected TermJRef<Object> createWeakReference(Term refId, Object o) {
		return new TermJRef<Object>(this, refId, o, (ReferenceQueue<Object>) getReferenceQueue());
	}
	
}
