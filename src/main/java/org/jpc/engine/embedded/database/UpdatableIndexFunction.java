package org.jpc.engine.embedded.database;

import java.util.Collection;
import java.util.Objects;
import java.util.function.Function;

import org.jpc.internal.collections.CollectionsUtil;
import org.jpc.term.Term;

public class UpdatableIndexFunction<T extends Term, U> implements Function<T, U> {

	private Function<T, U> indexFunction;
	private final Collection<IndexChangeListener> indexChangeListeners;
	
	public UpdatableIndexFunction() {
		this(null); //index function may be set later.
	}
	
	public UpdatableIndexFunction(Function<T, U> function) {
		this.indexFunction = function;
		indexChangeListeners = CollectionsUtil.createWeakSet();
	}
	
	public boolean isEnabled() {
		return indexFunction != null;
	}
	
	public void setIndexFunction(Function<T, U> indexFunction) {
		Objects.requireNonNull(indexFunction);
		if(!indexFunction.equals(this.indexFunction)) {
			this.indexFunction = indexFunction;
			notifyChangeListeners();
		}
	}
	
	//unfortunately not as simple as the implementation below (what should happen if there are more than one index associated to a term ?)
//	public void removeIndex() {
//		if(isEnabled()) {
//			indexFunction = null;
//			notifyListeners();
//		}
//	}

	
	@Override
	public U apply(T term) {
		return indexFunction.apply(term);
	}

	/**
	 * Notify the listeners that a new index has been associated to a functor.
	 * 
	 */
	private void notifyChangeListeners() {
		for(IndexChangeListener listener : indexChangeListeners) {
			listener.onIndexChange((UpdatableIndexFunction) this);
		}
	}
	
	public void addChangeListener(IndexChangeListener listener) {
		if(!indexChangeListeners.contains(listener))
			indexChangeListeners.add(listener); 
	}
	
	public void removeChangeListener(IndexChangeListener listener) {
		indexChangeListeners.remove(listener); 
	}
	
}
