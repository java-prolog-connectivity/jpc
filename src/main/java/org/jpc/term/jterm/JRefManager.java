package org.jpc.term.jterm;

import java.lang.ref.ReferenceQueue;

import org.jpc.term.Compound;

public class JRefManager extends IdentifiableWeakReferenceManager<JRefId, Object> {

	private static JRefManager defaultJRefManager;
	
	static {
		defaultJRefManager = new JRefManager(WeakReferencesCleaner.getDefaultReferenceQueue());
		WeakReferencesCleaner.startWeakReferencesCleaner();
	}
	
	public static JRefManager getJRefManager() {
		return defaultJRefManager;
	}
	
	public static Compound jRefTerm(Object o) {
		return jRef(o).asTerm();
	}
	
	public static <T> JRef<T> jRef(T o) {
		JRefId refId = JRefIdManager.getJRefIdManager().getOrCreate(o);
		return save(refId, o);
	}
	
	public static <T> JRef<T> save(JRefId refId, T o) {
		return (JRef<T>) defaultJRefManager.weakReference(refId, o);
	}
	
	public JRefManager(ReferenceQueue<?> referenceQueue){
		super(referenceQueue);
	}
	
	protected JRef<Object> createWeakReference(JRefId refId, Object o) {
		return new JRef<Object>(this, refId, o, (ReferenceQueue<Object>) getReferenceQueue());
	}

}
