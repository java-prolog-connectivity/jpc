package org.jpc.query;

import java.util.ArrayList;
import java.util.List;

import com.google.common.base.Function;
import com.google.common.base.Functions;

public class CursorAdapter<AdaptedType, AdapteeType> extends Cursor<AdaptedType> {

	private Cursor<AdapteeType> cursor;
	private Function<AdapteeType, AdaptedType> adapterFunction;
	
	public CursorAdapter(Cursor<AdapteeType> cursor) {
		this(cursor, (Function<AdapteeType, AdaptedType>) Functions.identity());
	}
	
	public CursorAdapter(Cursor<AdapteeType> cursor, Function<AdapteeType, AdaptedType> adapterFunction) {
		this.cursor = cursor;
		this.adapterFunction = adapterFunction;
	}

	@Override
	public boolean isAbortable() {
		return cursor.isAbortable();
	}
	
	/**
	 * The adaptee cursor could have optimized its basicOneSolution method.
	 * For example, a deterministic query can only open a cursor (i.e., with invocations to hasNext() and next()) bringing eagerly all the results.
	 * Therefore, this method overrides the default implementation of basicOneSolution() that makes use of hasNext() and next() and prefers to use 
	 * whatever mechanism is implemented by the adaptee cursor for bringing one solution.
	 */
	@Override
	protected AdaptedType basicOneSolutionOrThrow() {
		AdapteeType adaptee = cursor.oneSolutionOrThrow();
		return adaptee != null?adapterFunction.apply(adaptee):null;
	}
	
	/**
	 * The adaptee cursor could have optimized its basicAllSolutions method.
	 * For example, it could have brought all the results at once instead of lazily asking for them.
	 * Therefore, this method is overridden to profit from such optimization if existing
	 * 
	 */
	@Override
	protected List<AdaptedType> basicAllSolutions() {
		List<AdaptedType> allSolutions = new ArrayList<>();
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
	protected AdaptedType basicNext() {
		AdapteeType adaptee = cursor.next();
		return adaptee != null?adapterFunction.apply(adaptee):null;
	}
	
}
