package org.jpc.query;

import com.google.common.base.Function;

public class CursorAdapter<AdapterType, AdapteeType> extends Cursor<AdapterType> {

	private Cursor<AdapteeType> cursor;
	private Function<AdapteeType, AdapterType> converter;
	
	public CursorAdapter(Cursor<AdapteeType> cursor, Function<AdapteeType, AdapterType> converter) {
		this.cursor = cursor;
		this.converter = converter;
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
		return cursor.hasNext();
	}

	@Override
	public AdapterType next() {
		AdapteeType adaptee = cursor.next();
		return adaptee != null?converter.apply(adaptee):null;
	}
	
	@Override
	public void remove() {
		cursor.remove();
	}
}
