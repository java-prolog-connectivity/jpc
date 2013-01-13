package org.jpc.util.concurrent;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.jpc.engine.prolog.PrologEngine;
import org.jpc.engine.prolog.PrologEngineConfiguration;
import org.jpc.util.PrologEngineDependant;
import org.minitoolbox.concurrent.DelegatedExecutorService;
import org.minitoolbox.concurrent.OneThreadFactory;

public class OneThreadExecutor extends DelegatedExecutorService {

	private PrologEngine prologEngine;
	
	public OneThreadExecutor(final PrologEngineConfiguration prologEngineConfiguration) {
		super(Executors.newSingleThreadExecutor(new OneThreadFactory()));
		Future<PrologEngine> future = executor.submit(new PrologEngineInitializer(prologEngineConfiguration));
		try {
			prologEngine = future.get(); //in this way the logic engine is instantiated in the thread created by the thread factory
		} catch (InterruptedException | ExecutionException e) {
			throw new RuntimeException(e);
		}
	}
	
	@Override
	public void execute(Runnable task) {
		addPrologEngine(task);
		super.execute(task);
	}

    
    @Override
    public Future<?> submit(Runnable task) {
    	addPrologEngine(task);
        return super.submit(task);
    }
    
    @Override
    public <T> Future<T> submit(Callable<T> task) {
    	addPrologEngine(task);
        return super.submit(task);
    }
    
    @Override
    public <T> Future<T> submit(Runnable task, T result) {
    	addPrologEngine(task);
        return super.submit(task, result);
    }
    
    @Override
    public <T> List<Future<T>> invokeAll(Collection<? extends Callable<T>> tasks)
        throws InterruptedException {
    	addPrologEngine(tasks);
        return super.invokeAll(tasks);
    }
    
    @Override
    public <T> List<Future<T>> invokeAll(Collection<? extends Callable<T>> tasks,
                                         long timeout, TimeUnit unit)
        throws InterruptedException {
    	addPrologEngine(tasks);
        return super.invokeAll(tasks, timeout, unit);
    }
    
    @Override
    public <T> T invokeAny(Collection<? extends Callable<T>> tasks)
        throws InterruptedException, ExecutionException {
    	addPrologEngine(tasks);
        return super.invokeAny(tasks);
    }
    
    @Override
    public <T> T invokeAny(Collection<? extends Callable<T>> tasks,
                           long timeout, TimeUnit unit)
        throws InterruptedException, ExecutionException, TimeoutException {
    	addPrologEngine(tasks);
        return super.invokeAny(tasks, timeout, unit);
    }


    private void addPrologEngine(Object o) {
    	if(o instanceof PrologEngineDependant)
    		((PrologEngineDependant)o).setPrologEngine(prologEngine);
    	if(o instanceof Iterable) {
    		for(Object item : ((Iterable)o)) {
    			addPrologEngine(item);
    		}
    	}
    }

}
