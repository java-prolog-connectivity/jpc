package org.jpc.query;

import com.google.common.base.Predicate;

public class CursorFilter<T> extends Cursor<T> {

	private Cursor<T> cursor;
	private Predicate<T> predicate;
	
	public CursorFilter(Cursor<T> cursor, Predicate<T> predicate) {
		this.cursor = cursor;
		this.predicate = predicate;
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
	protected void basicRewind() {
		cursor.rewind();
	}

	@Override
	protected T basicNext() {
		while(cursor.hasNext()) {
			T t = cursor.next();
			if(predicate.apply(t))
				return t;
			
		}
		return null;
	}

}
