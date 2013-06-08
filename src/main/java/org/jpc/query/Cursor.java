package org.jpc.query;

import static com.google.common.base.Preconditions.checkArgument;
import static org.jpc.query.CursorState.EXHAUSTED;
import static org.jpc.query.CursorState.OPEN;
import static org.jpc.query.CursorState.READY;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import com.google.common.base.Function;
import com.google.common.base.Optional;
import com.google.common.base.Predicate;
/**
 * Note: The class hierarchy of class Cursor will be redesigned after the coming release of Java8 supporting virtual extension methods
 * @author sergioc
 *
 * @param <T>
 */
public abstract class Cursor<T> implements AutoCloseable, Iterator<T> {

	private CursorState state;
	private T cachedNext;
	
	public Cursor() {
		setState(READY);
	}
	
	public synchronized CursorState getState() {
		return state;
	}
	
	protected void setState(CursorState state) {
		this.state = state;
	}
	
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
		try {
			oneSolutionOrThrow();
			return true;
		} catch(NoSuchElementException e) {
			return false;
		}
	}
	
	/**
	 * This method will change to return a java.util.Optional when Java8 is ready
	 * @return an Optional value with the first solution. Empty means that there are no solutions.
	 */
	public Optional<T> oneSolution() {
		try {
			return Optional.of(oneSolutionOrThrow());
		} catch(NoSuchElementException e) {
			return Optional.absent();
		}
	}
	
	/**
	 * @precondition state = READY
	 * @postcondition state = READY
	 * will throw an exception if the cursor state is not READY
	 * @return
	 */
	public synchronized T oneSolutionOrThrow() {
		open();
		try {
			return basicOneSolutionOrThrow();
		} catch(NoSuchElementException e) {
			setState(EXHAUSTED);
			throw e;
		} finally {
			close();
		} 
	}

	protected T basicOneSolutionOrThrow() {
		return basicNext(); //will throw a NoSuchElementException in there are no solutions
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
		try (Cursor<T> autoCloseable = this) {
			long count = 0;
			while(count<to) {
				T solution = null;
				if(cachedHasNext()) {
					solution = cachedNext();
					if(count >= from)
						solutions.add(solution);
					count++;
				} else {
					break;
					//throw new IndexOutOfBoundsException("The cursor " + this + "has only " + count + " solutions");
				}
			}
		}
		return solutions;
	}
	
	/**
	 * @precondition state = READY
	 * @postcondition state = READY
	 * answers the cursor's all results
	 * The cursor should not be open when this method is called
	 */
	public synchronized List<T> allSolutions() {
		open();
		try (Cursor<T> autoCloseable = this) {
			return basicAllSolutions();
		}
	}

	/**
	 * The default implementation for obtaining all the solutions consists on just making use of the existing next() method
	 * However, children could override this in case many calls to next() are more expensive that obtaining all the results of the query at once (e.g., a findall/3 query in Prolog)
	 * @return
	 */
	protected List<T> basicAllSolutions() {
		List<T> allSolutions = new ArrayList<>();
		while (cachedHasNext()) { 
			allSolutions.add(cachedNext());
		}
		return allSolutions;
	}
	
	public Cursor<T> filter(Predicate<T> predicate) {
		return new CursorFilter<>(this, predicate);
	}
	
	public <T2>Cursor<T2> adapt(Function<T,T2> converter) {
		return new CursorAdapter<>(this, converter);
	}

	/**
	 * @return	true if the cursor is ready, otherwise false.
	 */
	public boolean isReady() {
		return getState().equals(READY);
	}
	
	/**
	 * @return	true if the cursor is open, otherwise false.
	 */
	public boolean isOpen() {
		return getState().equals(OPEN);
	}

	public boolean isExhausted() {
		return getState().equals(EXHAUSTED);
	}
	
	/**
	 * Aborts execution.
	 * This method is not synchronized since it is supposed to be called from a thread different to the one currently executing a query
	 * @throws InvalidCursorStateException if the cursor is not open
	 */
	public void abort() {
		if(!isOpen())
			throw new InvalidCursorStateException();
		basicAbort();
		close();
	}
	
	public abstract boolean isAbortable();
	
	private void open() {
		if(!isReady())
			throw new InvalidCursorStateException();
		setState(OPEN);
	}
	
	public synchronized void close() {
		if(!isReady()) { //there is no need to close if the state is READY
			basicClose();
			cachedNext = null;
			setState(READY);
		}
	}
	
	/**
	 * Answers if there are still rows in the cursor.
	 * This method may have effects on the state of the cursor. If the cursor is in READY state, it will change it to OPEN.
	 * If there are no more elements, it will change the state of the cursor to EXHAUSTED.
	 * @return true if there are more solutions to the query, false otherwise.
	 * 
	 */
	public synchronized boolean hasNext() {
		return cachedHasNext();
	}

	/**
	 * Answers the next solution.
	 * @return the next solution
	 * @throws NoSuchElementException if there are no more elements in the cursor.
	 */
	public synchronized T next() {
		return cachedNext();
	}
	
	private boolean cachedHasNext() {
		if(cachedNext != null)
			return true;
		try {
			cachedNext = cachedNext();
			return true;
		} catch(NoSuchElementException e) {
			return false;
		}
	}
	
	private T cachedNext() {
		if(isExhausted())
			throw new NoSuchElementException();
		if(isReady())
			open();
		T t;
		if(cachedNext != null) { //in the event of a previous call to cachedHasNext()
			t = cachedNext;
			cachedNext = null;
		} else {
			try {
				t = basicNext();
			} catch(NoSuchElementException e) {
				setState(EXHAUSTED);
				throw e;
			}	
		}
		return t;
	}
	
	protected abstract void basicAbort();
	
	protected abstract void basicClose();

	/**
	 * Returns the next available element in the cursor.
	 * If the cursor is exhausted it should throw a NoSuchElementException the first time it is invoked. 
	 * Its behavior is not specified if invoked more than once after the cursor has been exhausted.
	 * @return
	 */
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
