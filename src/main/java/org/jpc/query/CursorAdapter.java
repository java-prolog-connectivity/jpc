package org.jpc.query;

import com.google.common.base.Function;

public class CursorAdapter<AdapterType, AdapteeType> extends Cursor<AdapterType> {

	private Cursor<AdapteeType> cursor;
	private Function<AdapteeType, AdapterType> adapterFunction;

	protected static final Function<?, ?> defaultAdapterFunction = new Function<Object, Object>() {
		@Override
		public Object apply(Object object) {
			return object;
		}
	};
	
	public CursorAdapter(Cursor<AdapteeType> cursor) {
		this(cursor, (Function<AdapteeType, AdapterType>) defaultAdapterFunction);
	}
	
	public CursorAdapter(Cursor<AdapteeType> cursor, Function<AdapteeType, AdapterType> adapterFunction) {
		this.cursor = cursor;
		this.adapterFunction = adapterFunction;
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
	protected AdapterType basicNext() {
		AdapteeType adaptee = cursor.next();
		return adaptee != null?adapterFunction.apply(adaptee):null;
	}
	
}
