package org.jpc.term.jterm;

import static java.util.Arrays.asList;

import java.lang.ref.ReferenceQueue;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.Set;

import org.jpc.JpcException;
import org.jpc.engine.embedded.JpcEngine;
import org.jpc.engine.embedded.database.IndexDescriptor;
import org.jpc.engine.embedded.database.IndexManager;
import org.jpc.query.Query;
import org.jpc.query.Solution;
import org.jpc.term.Compound;
import org.jpc.term.Functor;
import org.jpc.term.JRef;
import org.jpc.term.Var;

import com.google.common.base.Optional;
import com.google.common.collect.MapMaker;

/**
 * Instances of this class allow to create term representation of arbitrary Java objects.
 * This term representations can be explicitly provided by a programmer (they must be compound terms),
 * or they may be explicitly generated if no provided. In that case, references will have the form 'jterm(id)', where id uniquely identifies the reference (in the current class loader).
 * @author sergioc
 *
 */
public class JTermManager {
	
	private static final JTermManager defaultJTermManager;
	
	static {
		defaultJTermManager = createDefault();
		WeakReferencesCleaner.startWeakReferencesCleaner();
	}
	
	/**
	 * @return the default JTermManager
	 */
	public static JTermManager getDefault() {
		return defaultJTermManager;
	}
	
	public static JTermManager createDefault() {
		return new JTermManager(WeakReferencesCleaner.getWeakReferencesCleaner().getReferenceQueue());
	}
	
	/**
	 * This map associates objects with their reference ids.
	 * The map discards an entry if the key (the object associated with a reference id) is marked for garbage collection.
	 */
	private final Map<Object, JTermRef<?>> currentRefsMap; //weak keys map.
	private final Set<Object> storedReferences; //set storing remembered references.
	private final JpcEngine embeddedEngine;
	private final ReferenceQueue<?> referenceQueue; //weak references created by the JTermManager will be instantiated using this reference queue.
	
	public JTermManager(ReferenceQueue<?> referenceQueue) {
		currentRefsMap = new MapMaker().weakKeys().makeMap();
		storedReferences = Collections.newSetFromMap(new IdentityHashMap<Object, Boolean>()); //reference set (members are identified by their reference, not by a call to equals()).
		this.referenceQueue = referenceQueue;
		this.embeddedEngine = new JpcEngine();
		IndexManager indexManager = embeddedEngine.getIndexManager();
		indexManager.setIndexDescriptor( new Functor(JTermId.JTERM_FUNCTOR_NAME, 2), 
				IndexDescriptor.argumentIndexDescriptor(1, indexManager) );
	}

	public IndexManager getIndexManager() {
		return embeddedEngine.getIndexManager();
	}
	
	public ReferenceQueue<?> getReferenceQueue() {
		return referenceQueue;
	}

	private void remove(Compound compound) {
		embeddedEngine.retractOne(new Compound(JTermId.JTERM_FUNCTOR_NAME, asList(compound, Var.ANONYMOUS_VAR)));
	}
	
	private void remove(JTermRef<?> jTerm) {
		embeddedEngine.retractOne(new Compound(JTermId.JTERM_FUNCTOR_NAME, asList(Var.ANONYMOUS_VAR, new JRef(jTerm))));
	}
	
	private void put(Compound compound, JTermRef<?> jTerm) {
		embeddedEngine.assertz(new Compound(JTermId.JTERM_FUNCTOR_NAME, asList(compound, new JRef(jTerm))));
	}
	
	private JTermRef<?> get(Compound compound) {
		String jTermVarName = "X";
		Query query = embeddedEngine.query(new Compound(JTermId.JTERM_FUNCTOR_NAME, asList(compound, new Var(jTermVarName))));
		Optional<Solution> optSolution = query.oneSolution();
		if(optSolution.isPresent()) {
			JRef jRef = (JRef) optSolution.get().get(jTermVarName);
			return (JTermRef<?>) jRef.getRef();
		}
		else
			return null;
	}
	
	
	
	
	/**
	 * Maps an object to a given term representation.
	 * @param o the object to express as a JTermRef reference associated with the term sent as second argument.
	 * @param compound the term representation of the object sent as first argument.
	 * @return a JTermRef reference on the object sent as second argument, uniquely identified by the term sent as first argument.
	 */
	private <T>JTermRef<T> newWeakJTermRef(T o, final Compound compound) {
		JTermRef<?> jTermRef = currentRefsMap.get(o);
		if(jTermRef == null) {
			jTermRef = get(compound);
			if(jTermRef == null) {
				Runnable cleaningTask = new Runnable() {
					@Override
					public void run() {
						remove(compound);
					}
				};
				jTermRef = new JTermRef<Object>(o, (ReferenceQueue<Object>) getReferenceQueue(), compound, cleaningTask);
				currentRefsMap.put(o, jTermRef);
				put(compound, jTermRef);
			} else {
				if(jTermRef.get() != o)
					throw new JpcException("Term reference id " + compound + " is already registered with the object: " + jTermRef.get() + ".");
			}
		} else {
			if(!compound.equals(jTermRef.asTerm()))
				throw new JpcException("Reference " + o + " is already registered with the term reference id: " + jTermRef.asTerm() + ".");
		}
		return (JTermRef<T>) jTermRef;
	}
	
	/**
	 * Maps an object to a compound term of the form jterm(id). id is an arbitrary integer uniquely identifying an object reference.
	 * @param o the object to express as a JTermRef reference.
	 * @return a JTermRef reference on the object sent as argument.
	 */
	private <T> JTermRef<T> newWeakJTermRef(T o) {
		JTermId refId = JTermIdManager.getJRefIdManager().getOrCreate(o);
		return newWeakJTermRef(o, refId.asTerm());
	}
	
	/**
	 * Maps an object to a given term representation.
	 * @param ref the object to express as the term sent as second argument.
	 * @param compound the term representation of the object sent as first argument.
	 * @return the term representation of the object (the second parameter).
	 */
	public synchronized Compound newWeakJTerm(Object ref, Compound compound) {
		return newWeakJTermRef(ref, compound).asTerm();
	}
	
	/**
	 * 
	 * @param ref the object to express as a term reference.
	 * @return the term representation of a reference.
	 */
	public synchronized Compound newWeakJTerm(Object ref) {
		return newWeakJTermRef(ref).asTerm();
	}
	
	
	public synchronized Compound newJTerm(Object ref, Compound compound) {
		Compound term = newWeakJTermRef(ref, compound).asTerm();
		storedReferences.add(ref);
		return term;
	}
	
	public synchronized Compound newJTerm(Object ref) {
		Compound term = newWeakJTermRef(ref).asTerm();
		storedReferences.add(ref);
		return term;
	}

	public synchronized void forgetJTerm(Compound term) {
		JTermRef<?> jTermRef = get(term);
		if(jTermRef != null) {
			Object ref = jTermRef.get();
			jTermRef.cleanUp(); //should remove the reference from the embedded Prolog engine.
			jTermRef.clear(); //so the reference will not be enqueued. Note that this is done before actually deleting remaining references, otherwise the GC may enqueue the reference before reaching this instruction.
			if(ref != null) {
				currentRefsMap.remove(ref);
				storedReferences.remove(ref);
			}
		}
	}
	
	public synchronized void forgetJTermRef(Object ref) {
		JTermRef<?> jTermRef = currentRefsMap.get(ref);
		if(jTermRef != null)
			forgetJTerm(jTermRef.asTerm());
	}

	public synchronized Compound jTerm(Object o) {
		Compound term = null;
		JTermRef<?> jTermRef = currentRefsMap.get(o);
		if(jTermRef != null)
			term = jTermRef.asTerm();
		return term;
	}
	
	public synchronized Compound jTermOrThrow(Object o) {
		Compound term = jTerm(o);
		if(term == null)
			throw new JpcException("No term representation associated with: " + o + ".");
		return term;
	}
	
	
	public synchronized Object resolve(Compound compound) {
		Object resolved = null;
		JTermRef<?> jTerm = get(compound);
		if(jTerm != null) {
			resolved = jTerm.get();
			if(resolved == null)
				throw new JpcException("Reference expired: " + compound + ".");
		}
		return resolved;
	}
	
	public synchronized Object resolveOrThrow(Compound compound) {
		Object resolved = resolve(compound);
		if(resolved == null)
			throw new JpcException("No reference associated with: " + compound + ".");
		return resolved;
	}

}
