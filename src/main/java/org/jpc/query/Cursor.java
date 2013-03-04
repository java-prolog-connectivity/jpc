package org.jpc.query;

import static com.google.common.base.Preconditions.checkArgument;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.jpc.JpcException;

import com.google.common.base.Function;
import com.google.common.base.Predicate;

public abstract class Cursor<T> implements AutoCloseable, Iterator<T> {

		public synchronized boolean hasSolution() {
			return oneSolution() != null;
		}
		
		public synchronized long numberOfSolutions() {
			return allSolutions().size();
		}
		
		public synchronized T oneSolution() {
			try {
				return nSolutions(1).get(0);
			} catch(IndexOutOfBoundsException e) {
				return null;
			}
		}
		
		/**
		 * 
		 * @param n the number of solutions the method should return
		 * @return is the first n solutions
		 */
		public synchronized List<T> nSolutions(long n) {
			return solutionsRange(0, n);
		}


		/**
		 * 
		 * @param from the (0-based) index (inclusive) of the first solution
		 * @param to the (0-based) index (exclusive) of the last solution
		 * @return a list with the solutions according to the indexes sent as parameters
		 */
		public synchronized List<T> solutionsRange(long from, long to) {
			checkArgument(from >= 0);
			checkArgument(to > from);
			List<T> solutions = new ArrayList<>();
			if (isOpen()) {
				throw new JpcException("Cursor is already open");
			} else {
				try (Cursor cursorToClose = this) {
					long count = 0;
					while(count<to) {
						T solution = null;
						if(hasNext()) {
							solution = next();
							if(count >= from)
								solutions.add(solution);
						}
						else
							throw new IndexOutOfBoundsException("The cursor " + this + "has only " + count + " solutions");
						count++;
					}
				}
				return solutions;
			}
		}
		
		/**
		 * answers the cursor's all results
		 * The cursor should not be open when this method is called
		 */
		public synchronized List<T> allSolutions() {
			if (isOpen()) {
				throw new JpcException("Cursor is already open");
			} else {
				List<T> allSolutions = new ArrayList<>();
				while (hasNext()) { 
					allSolutions.add(next());
				}
				return allSolutions;
			}
		}

		public synchronized Cursor<T> filter(Predicate<T> predicate) {
			return new CursorFilter<>(this, predicate);
		}
		
		public synchronized <T2>Cursor<T2> adapt(Function<T,T2> converter) {
			return new CursorAdapter<>(this, converter);
		}

		/**
		 * isOpen() returns true iff the cursor is open.
		 * @return	true if the cursor is open, otherwise false.
		 */
		public abstract  boolean isOpen();

		public abstract void abort();
		//public abstract void open();
		public abstract void close();
		
		/**
		 * Answers if there are still rows in the cursor
		 * In case there are no more solutions, the cursor will be closed by this method
		 * @return true if there are more solutions to the query
		 */
		public abstract boolean hasNext();

		
		/**
		 * Answers the next solution
		 * hasNext should be called before
		 * @return the next solution
		 */
		public abstract T next();

		@Override
		public void remove() {
			throw new UnsupportedOperationException();
		}
	}
