package org.jpc.util.concurrent;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.jpc.engine.prolog.PrologEngineConfiguration;
import org.minitoolbox.concurrent.DelegatedExecutorService;
import org.minitoolbox.concurrent.ThreadFactoryObserver;

public class JpcExecutor extends DelegatedExecutorService implements ThreadFactoryObserver {

	private final PrologEngineConfiguration prologEngineConfiguration;
	
	public JpcExecutor(PrologEngineConfiguration prologEngineConfiguration) {
		this(Executors.newSingleThreadExecutor(), prologEngineConfiguration);
	}
	
	public JpcExecutor(ExecutorService executor, PrologEngineConfiguration prologEngineConfiguration) {
		super(executor);
		this.prologEngineConfiguration = prologEngineConfiguration;
	}
	
	@Override public void onNewThreadCreated() {
		
	}
	
	@Override
	public void execute(Runnable task) {
		super.execute(wrap(task));
	}

    
    @Override
    public Future<?> submit(Runnable task) {
        return super.submit(wrap(task));
    }
    
    @Override
    public <T> Future<T> submit(Callable<T> task) {
        return super.submit(wrap(task));
    }
    
    @Override
    public <T> Future<T> submit(Runnable task, T result) {
        return super.submit(wrap(task), result);
    }
    
    @Override
    public <T> List<Future<T>> invokeAll(Collection<? extends Callable<T>> tasks)
        throws InterruptedException {
        return super.invokeAll(wrap(tasks));
    }
    
    @Override
    public <T> List<Future<T>> invokeAll(Collection<? extends Callable<T>> tasks,
                                         long timeout, TimeUnit unit)
        throws InterruptedException {
        return super.invokeAll(wrap(tasks), timeout, unit);
    }
    
    @Override
    public <T> T invokeAny(Collection<? extends Callable<T>> tasks)
        throws InterruptedException, ExecutionException {
        return super.invokeAny(wrap(tasks));
    }
    
    @Override
    public <T> T invokeAny(Collection<? extends Callable<T>> tasks,
                           long timeout, TimeUnit unit)
        throws InterruptedException, ExecutionException, TimeoutException {
        return super.invokeAny(wrap(tasks), timeout, unit);
    }

    
    private Runnable wrap(Runnable runnable) {
    	if(runnable instanceof JpcRunnable)
    		return new RunnablePrologEngineProvider(prologEngineConfiguration, (JpcRunnable)runnable);
    	else
    		return runnable;
    }
    
    private <T> Callable<T> wrap(Callable<T> callable) {
    	if(callable instanceof JpcCallable)
    		return new CallablePrologEngineProvider<T>(prologEngineConfiguration, (JpcCallable)callable);
    	else
    		return callable;
    }
    
    private <T> Collection<? extends Callable<T>> wrap(Collection<? extends Callable<T>> tasks) {
    	List<Callable<T>> wrapped = new ArrayList<>();
    	for(Callable<T> task : tasks) {
    		wrapped.add(task);
    	}
    	return wrapped;
    }

}