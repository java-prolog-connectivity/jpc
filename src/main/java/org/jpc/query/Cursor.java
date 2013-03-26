package org.jpc.query;

import static com.google.common.base.Preconditions.checkArgument;
import static org.jpc.query.CursorState.CLOSED;
import static org.jpc.query.CursorState.EXHAUSTED;
import static org.jpc.query.CursorState.OPEN;
import static org.jpc.query.CursorState.READY;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import com.google.common.base.Function;
import com.google.common.base.Predicate;

public abstract class Cursor<T> implements AutoCloseable, Iterator<T> {

	private CursorState state = READY;
	private T next;
	
	/**
	 * @precondition state = READY
	 * @postcondition state = READY
	 * will throw an exception if the cursor state is not READY
	 * @return
	 */
	public synchronized long numberOfSolutions() {
		return allSolutions().size(); 
	}

	/**
	 * @precondition state = READY
	 * @postcondition state = READY
	 * will throw an exception if the cursor state is not READY
	 * @return
	 */
	public synchronized boolean hasSolution() { 
		return oneSolution() != null;
	}
	
	/**
	 * @precondition state = READY
	 * @postcondition state = READY
	 * will throw an exception if the cursor state is not READY
	 * @return
	 */
	public synchronized T oneSolution() { 
		try {
			return nSolutions(1).get(0);
		} catch(IndexOutOfBoundsException e) {
			return null;
		}
	}

	/**
	 * @precondition state = READY
	 * @postcondition state = READY
	 * will throw an exception if the cursor state is not READY
	 * @param n the number of solutions the method should return or until exhaustion of the solutions (whatever happens first).
	 * @return is the first n solutions
	 */
	public synchronized List<T> nSolutions(long n) {
		return solutionsRange(0, n);
	}


	/**
	 * @precondition state = READY
	 * @postcondition state = READY
	 * @param from the (0-based) index (inclusive) of the first solution
	 * @param to the (0-based) index (exclusive) of the last solution
	 * @return a list with the solutions starting from 'from' (inclusive) to 'to' (exclusive) or until exhaustion of the solutions (whatever happens first). If there are less than (from+1) solutions will return an empty List
	 * 
	 * according to the indexes sent as parameters
	 */
	public synchronized List<T> solutionsRange(long from, long to) {
		if(!isReady())
			throw new InvalidCursorStateException();
		checkArgument(from >= 0);
		checkArgument(to > from);
		List<T> solutions = new ArrayList<>();
		try (Cursor cursorToClose = this) {
			long count = 0;
			while(count<to) {
				T solution = null;
				if(hasNext()) {
					solution = next();
					if(count >= from)
						solutions.add(solution);
					count++;
				} else {
					break;
					//throw new IndexOutOfBoundsException("The cursor " + this + "has only " + count + " solutions");
				}
			}
		}
		rewind();
		return solutions;
	}
	
	/**
	 * @precondition state = READY
	 * @postcondition state = READY
	 * answers the cursor's all results
	 * The cursor should not be open when this method is called
	 */
	public synchronized List<T> allSolutions() {
		if(!isReady()) {
			throw new InvalidCursorStateException();
		} else {
			List<T> allSolutions = basicAllSolutions();
			rewind();
			return allSolutions;
		}
	}

	/**
	 * The default implementation for obtaining all the solutions consists on just making use of the existing next() method
	 * However, children could override this in case many calls to next() are more expensive that obtaining all the results of the query at once (e.g., a findall/3 query in Prolog)
	 * @return
	 */
	protected List<T> basicAllSolutions() {
		List<T> allSolutions = new ArrayList<>();
		while (hasNext()) { 
			allSolutions.add(next());
		}
		return allSolutions;
	}
	
	public synchronized Cursor<T> filter(Predicate<T> predicate) {
		return new CursorFilter<>(this, predicate);
	}
	
	public synchronized <T2>Cursor<T2> adapt(Function<T,T2> converter) {
		return new CursorAdapter<>(this, converter);
	}

	/**
	 * @return	true if the cursor is ready, otherwise false.
	 */
	public boolean isReady() {
		return state.equals(READY);
	}
	
	/**
	 * @return	true if the cursor is open, otherwise false.
	 */
	public boolean isOpen() {
		return state.equals(OPEN);
	}

	/**
	 * @return	true if the cursor is closed, otherwise false.
	 */
	public boolean isClosed() {
		return state.equals(CLOSED);
	}

	public boolean isExhausted() {
		return state.equals(EXHAUSTED);
	}
	
	public synchronized void abort() {
		if(!isOpen())
			throw new InvalidCursorStateException();
		basicAbort();
		close();
	}
	
	public synchronized void close() {
		basicClose();
		next = null;
		state = CLOSED;
	}
	
	public synchronized void rewind() {
		basicRewind();
		next = null;
		state = READY;
	}
	
	/**
	 * @precondition state != CLOSED
	 * Answers if there are still rows in the cursor
	 * In case there are no more solutions, the cursor will be closed by this method
	 * @return true if there are more solutions to the query
	 */
	public synchronized boolean hasNext() {
		if(isClosed())
			throw new InvalidCursorStateException();
		if(isExhausted())
			return false;
		if(next == null) {
			try {
				next = next();
			} catch(NoSuchElementException e) {
				return false;
			}
		}
		return true;
	}

	
	/**
	 * Answers the next solution
	 * hasNext should be called before
	 * @return the next solution
	 */
	public synchronized T next() {
		if(isClosed())
			throw new InvalidCursorStateException();
		if(isExhausted())
			throw new NoSuchElementException();
		if(isReady())
			state = OPEN;
		T t;
		if(next != null) {
			t = next;
			next = null;
		} else {
			try {
				t = basicNext();
			} catch(NoSuchElementException e) {
				state = EXHAUSTED;
				throw e;
			}	
		}
		return t;
	}

	
	protected abstract void basicAbort();
	
	protected abstract void basicClose();
	
	protected abstract void basicRewind();

	protected abstract T basicNext();



	@Override
	public void finalize() {
		if(isOpen())
			basicClose();
	}

	@Override
	public void remove() {
		throw new UnsupportedOperationException();
	}

}
