package org.jpc.term.jterm;

import org.jpc.term.Compound;

public class JRefManager<REF_TYPE> extends IdentifiableWeakReferenceManager<JRefId, REF_TYPE> {

	private static JRefManager<Object> defaultRefManager;
	
	static {
		defaultRefManager = new JRefManager<>();
		new WeakReferencesCleaner(defaultRefManager, Thread.MAX_PRIORITY).start();
	}
	
	public static JRefManager<Object> getDefaultRefManager() {
		return defaultRefManager;
	}
	
	public static <T> JRef<T> jRef(Object o) {
		return (JRef<T>) defaultRefManager.newJRef(o);
	}
	
	public static Compound jRefTerm(Object o) {
		return jRef(o).asTerm();
	}
	
	public JRef<REF_TYPE> newJRef(REF_TYPE o) {
		JRef<REF_TYPE> jRef = new JRef<REF_TYPE>(o, getReferenceQueue());
		JRefId refId = jRef.getRefId();
		if(!containsKey(refId))
			put(refId, jRef);
		return jRef;
	}

}
