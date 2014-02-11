package org.jpc.term.jterm;

import static java.util.Arrays.asList;

import java.lang.ref.ReferenceQueue;
import java.util.Map;

import org.jpc.JpcException;
import org.jpc.engine.embedded.JpcEngine;
import org.jpc.engine.embedded.database.IndexDescriptor;
import org.jpc.engine.embedded.database.MutableIndexManager;
import org.jpc.query.Query;
import org.jpc.query.Solution;
import org.jpc.term.Compound;
import org.jpc.term.Functor;
import org.jpc.term.JRef;
import org.jpc.term.JRef.WeakJRef;
import org.jpc.term.Var;
import org.minitoolbox.reference.CleanableWeakReference;
import org.minitoolbox.reference.ReferenceType;
import org.minitoolbox.reference.ReferencesCleaner;

import com.google.common.base.Optional;
import com.google.common.collect.MapMaker;

/**
 * Allows to create term a representation (referred as a 'jterm') for arbitrary Java object references.
 * These term representations may be explicitly provided by a programmer (they must be compound terms),
 * or they may be generated under the curtains when no provided.
 * When generated, term references have the form 'jterm(id)', where id uniquely identifies the reference (in the current class loader).
 * @author sergioc
 *
 */
public class JTermManager {
	
	/**
	 * This dedicated cleaner executes cleaning tasks (in a separate thread) of weak and soft references that have been reclaimed.
	 * Such references defined a cleanUp() method invoked by the cleaner when the references have been garbage collected.
	 */
	private static ReferencesCleaner defaultJTermCleaner;

	/**
	 * References cleaner.
	 * @return the default references cleaner.
	 */
	private static synchronized ReferencesCleaner getJTermCleaner() {
		if(defaultJTermCleaner == null) {
			defaultJTermCleaner = new ReferencesCleaner(new ReferenceQueue<Object>());
			defaultJTermCleaner.start(); //the cleaner is started lazily (i.e., when it is requested for first time) in a new thread. The idea is to avoid creating a new thread if this feature is not used.
		}
		return defaultJTermCleaner;
	}
	
	public static final String JTERM_FUNCTOR_NAME = "jterm"; //references are stored in the embedded database as: jterm(Compound, JRef)

	/**
	 * This singleton is responsible to maintain weak references for which an id has been automatically generated.
	 * This ensures the contract that automatically-generated references are always the same for a given object, independently of the JTermManager employed.
	 * The association between a term and a weak reference persists until the reference is garbage collected.
	 * If a term cannot be resolved by a jTermManager, this singleton manager is queried.
	 * 
	 */
	private static final JTermManager weakJTermManager = new JTermManager(); 
	
	/**
	 * @return the weak JTermManager.
	 */
	private static JTermManager getWeakJTermManager() {
		return weakJTermManager;
	}
	
	/**
	 * @param ref an object to map to a term. 
	 * @return a term representation of the object. A weak reference to the object is internally maintained.
	 */
	public static Compound weakJTerm(Object ref) {
		return getWeakJTermManager().newWeakJTerm(ref);
	}

	/**
	 * @param ref an object to map to a term. 
	 * @return a term representation of the object. A soft reference to the object is internally maintained.
	 */
	public static Compound softJTerm(Object ref) {
		return getWeakJTermManager().newSoftJTerm(ref);
	}
	

	private final JpcEngine embeddedEngine; //embedded Jpc Prolog engine.
	private final ReferenceQueue<?> referenceQueue; //weak references created by the JTermManager will be instantiated using this reference queue.
	/**
	 * This map associates objects with their jterm representation.
	 * Instantiated as a weak map, it discards an entry when the key (the object associated with the jterm) is marked for garbage collection.
	 */
	private final Map<Object, Compound> currentRefsMap; //weak keys map.
	
	
	public JTermManager() {
		this(getJTermCleaner().getReferenceQueue());
	}
	
	public JTermManager(ReferenceQueue<?> referenceQueue) {
		this.referenceQueue = referenceQueue;
		/**
		 * Quoting from the Guava documentation (http://docs.guava-libraries.googlecode.com/git-history/release/javadoc/com/google/common/collect/MapMaker.html) :
		 * 
		 * "Note: by default, the returned map uses equality comparisons (the equals method) to determine equality for keys or values. 
		 * However, if weakKeys() was specified, the map uses identity (==) comparisons instead for keys. 
		 * Likewise, if weakValues() or softValues() was specified, the map uses identity comparisons for values. "
		 * 
		 * Therefore, our map uses identity (reference) comparisons. 
		 * This is a desirable property, since we need different term ids (jterms) for objects with different references, and this is independent of the objects being equals according to their equals() methods.
		 */
		currentRefsMap = new MapMaker().weakKeys().makeMap();
		embeddedEngine = new JpcEngine();
		MutableIndexManager indexManager = embeddedEngine.getIndexManager();
		Functor jtermFunctor = new Functor(JTERM_FUNCTOR_NAME, 2);
		//IndexDescriptor indexDescriptor = IndexDescriptor.forArgument(1); //inefficient.
		IndexDescriptor firstArgumentIdx = IndexDescriptor.forIndexedArgument(1, indexManager); //makes use of any index defined for the first argument.
//		IndexDescriptor secondArgumentIdx = new IndexDescriptor(new Function<Term, Object>() {
//			@Override
//			public Object apply(Term term) {
//				Compound compound = (Compound) term;
//				JRef<Object> jRef = (JRef<Object>) compound.arg(2);
//				Object referent = jRef.getReferent();
//				Object key = null;
//				if(referent != null)
//					key = JTermIdManager.getDefault().jTermId(referent);
//				return key;
//			}
//		}); //index also by the referent in the second argument.
		//indexManager.setIndexDescriptors(jtermFunctor, asList(firstArgumentIdx, secondArgumentIdx)); //clause heads having jterm/2 as a functor will be indexed according to their first and second argument.
		indexManager.setIndexDescriptors(jtermFunctor, asList(firstArgumentIdx));
	}

	public MutableIndexManager getIndexManager() {
		return embeddedEngine.getIndexManager();
	}

	private void remove(Compound compound) {
		embeddedEngine.retractOne(new Compound(JTERM_FUNCTOR_NAME, asList(compound, Var.ANONYMOUS_VAR)));
	}
	
	private void remove(JRef<?> jref) {
		embeddedEngine.retractOne(new Compound(JTERM_FUNCTOR_NAME, asList(Var.ANONYMOUS_VAR, jref)));
	}
	
	private void put(Compound compound, JRef<?> jref) {
		embeddedEngine.assertz(new Compound(JTERM_FUNCTOR_NAME, asList(compound, jref)));
	}
	
	private <T> Optional<JRef<T>> getJRef(Compound compound) {
		String jRefVarName = "X";
		Query query = embeddedEngine.query(new Compound(JTERM_FUNCTOR_NAME, asList(compound, new Var(jRefVarName))));
		Optional<Solution> optSolution = query.oneSolution();
		JRef<T> jRef = null;
		if(optSolution.isPresent()) {
			jRef = (JRef<T>) optSolution.get().get(jRefVarName);
		}
		return Optional.fromNullable(jRef);
	}
	
//	private Optional<Compound> getJTerm(JRef<?> jref) {
//		String compoundVarName = "X";
//		Query query = embeddedEngine.query(new Compound(JTERM_FUNCTOR_NAME, asList(new Var(compoundVarName), jref)));
//		Optional<Solution> optSolution = query.oneSolution();
//		Compound compound = null;
//		if(optSolution.isPresent()) {
//			compound = (Compound) optSolution.get().get(compoundVarName);
//		}
//		return Optional.fromNullable(compound);
//	}
//	
//	private Optional<Compound> getJTermFromReferent(Object referent) {
//		return getJTerm((JRef<?>)JRef.jRef(referent));
//	}
	
	private <T> JRef<T> basicNewJRef(T ref, final Compound compound, ReferenceType type) {
		Runnable cleaningTask = new Runnable() {
			@Override
			public void run() {
				remove(compound);
			}
		};
		switch(type) {
		case STRONG:
			return JRef.jRef(ref);
		case SOFT:
			return JRef.softJRef(ref, (ReferenceQueue)referenceQueue, cleaningTask);
		case WEAK:
			return JRef.weakJRef(ref, (ReferenceQueue)referenceQueue, cleaningTask);
		default:
			throw new JpcException("Unrecognized reference type.");
		}
	}
	
	
	/**
	 * Maps an object to a given term representation.
	 * @param ref the object to identify as a jterm.
	 * @param compound the term representation (jterm) of the object sent as first argument.
	 * @return a JRef for the object sent as second argument, uniquely identified by the term sent as first argument.
	 */
	private <T> JRef<T> newJRef(T ref, Compound compound, ReferenceType type) {
		JRef<T> jref = null;
		Optional<JRef<T>> existingJRefOpt = getJRef(compound); //see if the compound has already been associated with a reference.
		if(!existingJRefOpt.isPresent()) {
			Compound existingJTerm = currentRefsMap.get(ref); //see if the reference has already been associated with a term.
			if(existingJTerm == null) {
				jref = basicNewJRef(ref, compound, type);
				put(compound, (JRef<T>)jref);
				currentRefsMap.put(ref, compound);
			} else {
				if(!existingJTerm.equals(compound))
					throw new JpcException("Reference " + ref + " is already registered with the term reference id: " + existingJTerm + ".");
			}
		} else {
			JRef<T> existingJRef = existingJRefOpt.get();
			T existingReferent = existingJRef.getReferent();
			if(existingReferent == null)
				throw new JpcException("Reference to: " + compound + " expired.");
			else {
				if(existingReferent != ref)
					throw new JpcException("Term reference id " + compound + " is already registered with the object: " + existingReferent + ".");
				else if(!existingJRef.getReferenceType().equals(type))
					throw new JpcException("Term reference id " + compound + " is already registered with the object: " + ref + " with reference type " + type + ".");
			}
		}
		return jref;
	}
	

	
	/**
	 * Maps an object to a given term representation.
	 * The mapping exists as long as the object is not garbage collected.
	 * The reference is internally maintained by means of a soft reference.
	 * If the object is already associated to the compound sent as parameter the method returns without errors.
	 * However, if the object or compound are already registered and associated to different objects (i.e., different to the ones in the method parameters), it will throw an exception.
	 * @param ref the object to identify as the term sent as second argument.
	 * @param compound the term representation of the object sent as first argument.
	 * @return the term representation of the object (the second parameter).
	 */
	public synchronized Compound newSoftJTerm(Object ref, Compound compound) {
		newJRef(ref, compound, ReferenceType.SOFT);
		return compound;
	}
	
	/**
	 * Maps an object to a generated and unique term representation.
	 * The mapping exists as long as the object is not garbage collected.
	 * The reference is internally maintained by means of a soft reference.
	 * @param ref the object to identify as a term reference.
	 * @return the (generated) term representation of a reference.
	 */
	public synchronized Compound newSoftJTerm(Object ref) {
		Compound refId = JTermIdManager.getDefault().newJTermId(ref);
		newSoftJTerm(ref, refId);
		if(this != getWeakJTermManager())
			getWeakJTermManager().newWeakJTerm(ref, refId); //registered in the global reference manager (the global register is always weak).
		return refId;
	}
	
	
	
	/**
	 * Maps an object to a given term representation.
	 * The mapping exists as long as the object is not garbage collected.
	 * The reference is internally maintained by means of a weak reference.
	 * If the object is already associated to the compound sent as parameter the method returns without errors.
	 * However, if the object or compound are already registered and associated to different objects (i.e., different to the ones in the method parameters), it will throw an exception.
	 * @param ref the object to identify as the term sent as second argument.
	 * @param compound the term representation of the object sent as first argument.
	 * @return the term representation of the object (the second parameter).
	 */
	public synchronized Compound newWeakJTerm(Object ref, Compound compound) {
		newJRef(ref, compound, ReferenceType.WEAK); //registered in this reference manager.
		return compound;
	}
	
	/**
	 * Maps an object to a generated and unique term representation.
	 * The mapping exists as long as the object is not garbage collected.
	 * The reference is internally maintained by means of a weak reference.
	 * @param ref the object to identify as a term reference.
	 * @return the (generated) term representation of a reference.
	 */
	public synchronized Compound newWeakJTerm(Object ref) {
		Compound refId = JTermIdManager.getDefault().newJTermId(ref);
		newWeakJTerm(ref, refId);
		if(this != getWeakJTermManager())
			getWeakJTermManager().newWeakJTerm(ref, refId); //registered in the global reference manager.
		return refId;
	}
	
	
	
	/**
	 * Maps an object to a given term representation.
	 * The mapping exists as long as the object is not garbage collected.
	 * The object reference is internally stored to prevent it from being garbage collected.
	 * @param ref the object to identify as the term sent as second argument.
	 * @param compound the term representation of the object sent as first argument.
	 * @return the term representation of the object (the second parameter).
	 */
	public synchronized Compound newJTerm(Object ref, Compound compound) {
		newJRef(ref, compound, ReferenceType.STRONG); //registered in this reference manager.
		return compound;
	}
	
	/**
	 * Maps an object to a generated and unique term representation.
	 * The mapping exists as long as the object is not garbage collected.
	 * The object reference is internally stored to prevent it from being garbage collected.
	 * @param ref the object to identify as a term reference.
	 * @return the (generated) term representation of a reference.
	 */
	public synchronized Compound newJTerm(Object ref) {
		Compound refId = JTermIdManager.getDefault().newJTermId(ref);
		newJTerm(ref, refId);
		if(this != getWeakJTermManager())
			getWeakJTermManager().newWeakJTerm(ref, refId); //registered in the global reference manager (the global register is always weak).
		return refId;
	}

	/**
	 * The given term will not be associated anymore to a reference.
	 * @param term the term representation of a reference to forget.
	 */
	public synchronized void forgetJTerm(Compound term) {
		Optional<JRef<Object>> jRefOpt = getJRef(term);
		if(jRefOpt.isPresent()) {
			JRef<Object> jRef = jRefOpt.get();
			Object referent = jRef.getReferent();
			if(referent != null) { //the reference is still alive
				currentRefsMap.remove(referent);
				if(jRef instanceof WeakJRef) {
					CleanableWeakReference<?> reference = (CleanableWeakReference<?>) ((WeakJRef)jRef).getReference();
					//reference.cleanUp();
					reference.clear(); //so the reference will not be enqueued. Note that this is done before actually deleting the reference, otherwise the GC may enqueue the reference before reaching the deletion instruction.
				}
			}
			remove(term); //remove the reference from the embedded Prolog engine.
		}
	}
	
	/**
	 * The given reference will not be associated anymore to a term.
	 * @param ref the reference to forget.
	 */
	public synchronized void forgetJTermRef(Object ref) {
		Compound compound = currentRefsMap.get(ref);
		if(compound != null)
			forgetJTerm(compound);
	}

	/**
	 * @param ref an object reference.
	 * @return the term representation of a given object reference. Null if the reference is not associated with a term representation.
	 */
	public synchronized Compound jTerm(Object ref) {
		return currentRefsMap.get(ref);
	}

	/**
	 * 
	 * @param ref an object reference.
	 * @return the term representation of a given object reference.
	 * @throws JpcException if the reference is not associated with a term representation.
	 */
	public synchronized Compound jTermOrThrow(Object ref) {
		Compound term = jTerm(ref);
		if(term == null)
			throw new JpcException("No term representation associated with: " + ref + ".");
		return term;
	}
	
	private <T> Optional<JRef<T>> jRefFromCompound(Compound compound) {
		Optional<JRef<T>> jRefOpt = getJRef(compound);
		if(!jRefOpt.isPresent() && this != getWeakJTermManager())
			jRefOpt = getWeakJTermManager().jRefFromCompound(compound);
		return jRefOpt;
	}
	
//	private Optional<Compound> jTermFromJTermRef(Object ref) {
//		Optional<Compound> jTermOpt = getJTermFromReferent(ref);
//		if(!jTermOpt.isPresent() && this != getWeakJTermManager())
//			jTermOpt = getWeakJTermManager().jTermFromRef(ref);
//		return jTermOpt;
//	}
	
	/**
	 * 
	 * @param compound a (compound) term.
	 * @return the reference associated with the given term. Null if no reference is associated with such a term.
	 */
	public synchronized <T> T resolve(Compound compound) {
		T referent = null;
		Optional<JRef<T>> jRefOpt = jRefFromCompound(compound);
		if(jRefOpt.isPresent()) {
			JRef<T> jRef = jRefOpt.get();
			referent = jRef.getReferent();
			if(referent == null)
				throw new JpcException("Reference expired: " + compound + ".");
		}
		return referent;
	}
	
	/**
	 * 
	 * @param compound a (compound) term.
	 * @return the reference associated with the given term.
	 * @throws JpcException if no reference is associated with such a term.
	 */
	public synchronized <T> T resolveOrThrow(Compound compound) {
		T resolved = resolve(compound);
		if(resolved == null)
			throw new JpcException("No reference associated with: " + compound + ".");
		return resolved;
	}

}
