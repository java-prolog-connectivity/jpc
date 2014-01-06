package org.jpc.engine.embedded.indexing;

import java.util.Collection;
import java.util.Objects;

import org.jpc.term.Term;
import org.minitoolbox.collections.CollectionsUtil;

import com.google.common.base.Function;

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
			notifyListeners();
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
	private void notifyListeners() {
		for(IndexChangeListener listener : indexChangeListeners) {
			listener.onIndexChange((UpdatableIndexFunction) this);
		}
	}
	
	public void addListener(IndexChangeListener listener) {
		if(!indexChangeListeners.contains(listener))
			indexChangeListeners.add(listener); 
	}
	
	public void removeListener(IndexChangeListener listener) {
		indexChangeListeners.remove(listener); 
	}
	
}
