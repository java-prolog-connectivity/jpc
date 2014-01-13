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
 * Instances of this class allow to create term representations of arbitrary Java object references.
 * These term representations may be explicitly provided by a programmer (they must be compound terms),
 * or they may be generated under the curtains if no provided. 
 * If generated, term references will have the form 'jterm(id)', where id uniquely identifies the reference (in the current class loader).
 * @author sergioc
 *
 */
public class JTermManager {
	
	public static final String JTERM_FUNCTOR_NAME = "jterm";
	private static final JTermManager weakJTermManager;
	
	static {
		WeakReferencesCleaner.startWeakReferencesCleaner();
		weakJTermManager = new JTermManager();
	}

	/**
	 * This term manager is a singleton that stores jpc-generated term representations associated with (weak) object references.
	 * The association persists until the reference is garbage collected.
	 * If a term cannot be resolved by a jTermManager, the singleton manager will be queried.
	 * @return the weak JTermManager.
	 */
	private static JTermManager getWeakJTermManager() {
		return weakJTermManager;
	}
	
	public static Compound weakJTerm(Object ref) {
		return getWeakJTermManager().newWeakJTerm(ref);
	}
	
	
	/**
	 * This map associates objects with their reference ids.
	 * This map discards an entry if the key (the object associated with the reference id) is marked for garbage collection.
	 */
	private final Map<Object, JTermRef<?>> currentRefsMap; //weak keys map.
	private final Set<Object> storedReferences; //set storing references so they will not be garbage collected.
	private final JpcEngine embeddedEngine; //embedded Jpc Prolog engine.
	private final ReferenceQueue<?> referenceQueue; //weak references created by the JTermManager will be instantiated using this reference queue.
	
	public JTermManager() {
		this(WeakReferencesCleaner.getWeakReferencesCleaner().getReferenceQueue());
	}
	
	public JTermManager(ReferenceQueue<?> referenceQueue) {
		/**
		 * Quoting from the Guava documentation (http://docs.guava-libraries.googlecode.com/git-history/release/javadoc/com/google/common/collect/MapMaker.html) :
		 * 
		 * "Note: by default, the returned map uses equality comparisons (the equals method) to determine equality for keys or values. 
		 * However, if weakKeys() was specified, the map uses identity (==) comparisons instead for keys. 
		 * Likewise, if weakValues() or softValues() was specified, the map uses identity comparisons for values. "
		 * 
		 * Therefore, our map uses identity (reference) comparisons. This is a desirable property, since we need different references ids for objects with different references, and this is independent of how these objects define their equals() methods.
		 */
		currentRefsMap = new MapMaker().weakKeys().makeMap();
		storedReferences = Collections.newSetFromMap(new IdentityHashMap<Object, Boolean>()); //reference set (members are identified by their references, not by a call to equals()).
		this.referenceQueue = referenceQueue;
		this.embeddedEngine = new JpcEngine();
		IndexManager indexManager = embeddedEngine.getIndexManager();
		indexManager.setIndexDescriptor( new Functor(JTERM_FUNCTOR_NAME, 2), 
				IndexDescriptor.argumentIndexDescriptor(1, indexManager) ); //clause heads having jterm as a functor name will be indexed according to the first argument of the term head.
	}

	
	public IndexManager getIndexManager() {
		return embeddedEngine.getIndexManager();
	}
	
	public ReferenceQueue<?> getReferenceQueue() {
		return referenceQueue;
	}

	private void remove(Compound compound) {
		embeddedEngine.retractOne(new Compound(JTERM_FUNCTOR_NAME, asList(compound, Var.ANONYMOUS_VAR)));
	}
	
	private void remove(JTermRef<?> jTerm) {
		embeddedEngine.retractOne(new Compound(JTERM_FUNCTOR_NAME, asList(Var.ANONYMOUS_VAR, new JRef(jTerm))));
	}
	
	private void put(Compound compound, JTermRef<?> jTerm) {
		embeddedEngine.assertz(new Compound(JTERM_FUNCTOR_NAME, asList(compound, new JRef(jTerm))));
	}
	
	private JTermRef<?> get(Compound compound) {
		String jTermVarName = "X";
		Query query = embeddedEngine.query(new Compound(JTERM_FUNCTOR_NAME, asList(compound, new Var(jTermVarName))));
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
	 * @param ref the object to express as a JTermRef associated with the term sent as second argument.
	 * @param compound the term representation of the object sent as first argument.
	 * @return a JTermRef reference for the object sent as second argument, uniquely identified by the term sent as first argument.
	 */
	private <T>JTermRef<T> newWeakJTermRef(T ref, final Compound compound) {
		JTermRef<?> jTermRef = currentRefsMap.get(ref);
		if(jTermRef == null) {
			jTermRef = get(compound);
			if(jTermRef == null) {
				Runnable cleaningTask = new Runnable() {
					@Override
					public void run() {
						remove(compound);
					}
				};
				jTermRef = new JTermRef<Object>(ref, (ReferenceQueue<Object>) getReferenceQueue(), compound, cleaningTask);
				currentRefsMap.put(ref, jTermRef);
				put(compound, jTermRef);
			} else {
				if(jTermRef.get() != ref)
					throw new JpcException("Term reference id " + compound + " is already registered with the object: " + jTermRef.get() + ".");
			}
		} else {
			if(!compound.equals(jTermRef.asTerm()))
				throw new JpcException("Reference " + ref + " is already registered with the term reference id: " + jTermRef.asTerm() + ".");
		}
		return (JTermRef<T>) jTermRef;
	}
	
	/**
	 * Maps an object to a compound term of the form jterm(id). id is an arbitrary integer uniquely identifying an object reference.
	 * @param ref the object to express as a JTermRef reference.
	 * @return a JTermRef reference for the object sent as argument.
	 */
	private <T> JTermRef<T> newWeakJTermRef(T ref) {
		Compound refId = WeakJTermIdManager.getDefault().newWeakJTerm(ref);
		getWeakJTermManager().newWeakJTermRef(ref, refId);
		return newWeakJTermRef(ref, refId);
	}
	
	
	/**
	 * Maps an object to a given term representation.
	 * The mapping exists as long as the object is not garbage collected.
	 * @param ref the object to express as the term sent as second argument.
	 * @param compound the term representation of the object sent as first argument.
	 * @return the term representation of the object (the second parameter).
	 */
	public synchronized Compound newWeakJTerm(Object ref, Compound compound) {
		return newWeakJTermRef(ref, compound).asTerm();
	}
	
	/**
	 * Maps an object to a generated and unique term representation.
	 * The mapping exists as long as the object is not garbage collected.
	 * @param ref the object to express as a term reference.
	 * @return the (generated) term representation of a reference.
	 */
	public synchronized Compound newWeakJTerm(Object ref) {
		return newWeakJTermRef(ref).asTerm();
	}
	
	/**
	 * Maps an object to a given term representation.
	 * The mapping exists as long as the object is not garbage collected.
	 * The object reference is internally stored to prevent it from being garbage collected.
	 * @param ref the object to express as the term sent as second argument.
	 * @param compound the term representation of the object sent as first argument.
	 * @return the term representation of the object (the second parameter).
	 */
	public synchronized Compound newJTerm(Object ref, Compound compound) {
		Compound term = newWeakJTermRef(ref, compound).asTerm();
		storedReferences.add(ref);
		return term;
	}
	
	/**
	 * Maps an object to a generated and unique term representation.
	 * The mapping exists as long as the object is not garbage collected.
	 * The object reference is internally stored to prevent it from being garbage collected.
	 * @param ref the object to express as a term reference.
	 * @return the (generated) term representation of a reference.
	 */
	public synchronized Compound newJTerm(Object ref) {
		Compound term = newWeakJTermRef(ref).asTerm();
		storedReferences.add(ref);
		return term;
	}

	/**
	 * The given term will not be associated anymore with a reference.
	 * @param term the term representation of a reference to forget.
	 */
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
	
	/**
	 * The given reference will not be associated anymore with a term.
	 * @param ref the reference to forget.
	 */
	public synchronized void forgetJTermRef(Object ref) {
		JTermRef<?> jTermRef = currentRefsMap.get(ref);
		if(jTermRef != null)
			forgetJTerm(jTermRef.asTerm());
	}

	/**
	 * @param ref an object reference.
	 * @return the term representation of a given object reference. Null if the reference is not associated with a term representation.
	 */
	public synchronized Compound jTerm(Object ref) {
		Compound term = null;
		JTermRef<?> jTermRef = currentRefsMap.get(ref);
		if(jTermRef != null)
			term = jTermRef.asTerm();
		return term;
	}

	public synchronized Compound jTermOrThrow(Object ref) {
		Compound term = jTerm(ref);
		if(term == null)
			throw new JpcException("No term representation associated with: " + ref + ".");
		return term;
	}
	
	private JTermRef<?> jTermRefFromCompound(Compound compound) {
		JTermRef<?> jTermRef = get(compound);
		if(jTermRef == null && this != getWeakJTermManager())
			jTermRef = getWeakJTermManager().jTermRefFromCompound(compound);
		return jTermRef;
	}
//	
//	private JTermRef<?> jTermRefFromRef(Object ref) {
//		JTermRef<?> jTermRef = currentRefsMap.get(ref);
//		if(jTermRef == null && this != getDefault())
//			jTermRef = getDefault().jTermRefFromRef(ref);
//		return jTermRef;
//	}
	
	/**
	 * 
	 * @param compound a (compound) term.
	 * @return the reference associated with the given term. Null if no reference is associated with such a term.
	 */
	public synchronized Object resolve(Compound compound) {
		Object resolved = null;
		JTermRef<?> jTermRef = jTermRefFromCompound(compound);
		if(jTermRef != null) {
			resolved = jTermRef.get();
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
