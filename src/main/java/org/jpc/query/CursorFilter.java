package org.jpc.query;

import java.util.ArrayList;
import java.util.List;

import java.util.function.Predicate;

public class CursorFilter<T> extends Cursor<T> {

	private Cursor<T> cursor;
	private Predicate<T> predicate;
	
	public CursorFilter(Cursor<T> cursor, Predicate<T> predicate) {
		this.cursor = cursor;
		this.predicate = predicate;
	}
	
	@Override
	public boolean isAbortable() {
		return cursor.isAbortable();
	}
	
	/**
	 * The filtered cursor could have optimized its basicAllSolutions method
	 * For example, it could have brought all the results at once instead of lazily asking for them
	 * Therefore, this method is overridden to profit from such optimization if existing
	 * 
	 */
	@Override
	protected List<T> basicAllSolutions() {
		List<T> allSolutions = new ArrayList<>();
		for(T t : cursor.allSolutions()) {
			if (predicate.test(t)) {
				allSolutions.add(t);
			}
		}
		return allSolutions;
	}
	
	@Override
	protected void basicAbort() {
		cursor.abort();
	}

	@Override
	protected void basicClose() {
		cursor.close();
	}

	@Override
	protected T basicNext() {
		while(cursor.hasNext()) {
			T t = cursor.next();
			if (predicate.test(t)) {
				return t;
			}
		}
		return null;
	}

}
