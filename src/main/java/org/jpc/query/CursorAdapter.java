package org.jpc.query;

import java.util.ArrayList;
import java.util.List;

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

	/**
	 * The adaptee cursor could have optimized its basicAllSolutions method
	 * For example, it could have brought all the results at once instead of lazily asking for them
	 * Therefore, this method is overridden to profit from such optimization if existing
	 * 
	 */
	@Override
	protected List<AdapterType> basicAllSolutions() {
		List<AdapterType> allSolutions = new ArrayList<>();
		for(AdapteeType adaptee : cursor.allSolutions()) {
			allSolutions.add(adapterFunction.apply(adaptee));
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
	protected void basicRewind() {
		cursor.rewind();
	}

	@Override
	protected AdapterType basicNext() {
		AdapteeType adaptee = cursor.next();
		return adaptee != null?adapterFunction.apply(adaptee):null;
	}
	
}
