package org.jpc.internal.concurrent;

import static com.google.common.base.Preconditions.checkNotNull;

public class ExecutionResult<T> {
	
	private final T result;
	private final Exception exception;
	
	public ExecutionResult(T result) {
		this.result = result;
		exception = null;
	}
	
	public ExecutionResult(Exception exception) {
		checkNotNull(exception);
		this.exception = exception;
		result = null;
	}
	
	public boolean isError() {
		return exception != null;
	}
	
	public T getResult() throws Exception {
		if(isError())
			throw exception;
		else
			return result;
	}
	
	public Exception getError() {
		return exception;
	}
	
}
