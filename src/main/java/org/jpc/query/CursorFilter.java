package org.jpc.query;

import com.google.common.base.Predicate;

public class CursorFilter<T> extends Cursor<T> {

	private T cachedNext;
	private Cursor<T> cursor;
	private Predicate<T> predicate;
	
	public CursorFilter(Cursor<T> cursor, Predicate<T> predicate) {
		this.cursor = cursor;
		this.predicate = predicate;
	}
	
	@Override
	public boolean isOpen() {
		return cursor.isOpen();
	}

	@Override
	public void abort() {
		cursor.abort();
	}

	@Override
	public void close() {
		cursor.close();
	}

	@Override
	public boolean hasNext() {
		while(cursor.hasNext()) {
			cachedNext = cursor.next();
			if(predicate.apply(cachedNext))
				return true;
		}
		cachedNext = null;
		return false;
	}

	@Override
	public T next() {
		return cachedNext;
	}
	
	@Override
	public void remove() {
		cursor.remove();
	}
	
}
