package org.jpc.internal.concurrent;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.AbstractExecutorService;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * Mostly a copy of the non-public Java class java.util.concurrent.Executors.DelegatedExecutorService.
 * (variations from the original class are commented out)
 * Source: http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/7-b147/java/util/concurrent/Executors.java
 * @author sergioc
 *
 */
public class DelegatedExecutorService extends AbstractExecutorService {
    protected final ExecutorService executor; //executor changed to be protected
    
    public DelegatedExecutorService(ExecutorService executor) { this.executor = executor; } //constructor changed to be public
    
    @Override
    public void execute(Runnable command) { executor.execute(command); }
    
    @Override
    public void shutdown() { executor.shutdown(); }
    
    @Override
    public List<Runnable> shutdownNow() { return executor.shutdownNow(); }
    
    @Override
    public boolean isShutdown() { return executor.isShutdown(); }
    
    @Override
    public boolean isTerminated() { return executor.isTerminated(); }
    
    @Override
    public boolean awaitTermination(long timeout, TimeUnit unit)
        throws InterruptedException {
        return executor.awaitTermination(timeout, unit);
    }
    
    @Override
    public Future<?> submit(Runnable task) {
        return executor.submit(task);
    }
    
    @Override
    public <T> Future<T> submit(Callable<T> task) {
        return executor.submit(task);
    }
    
    @Override
    public <T> Future<T> submit(Runnable task, T result) {
        return executor.submit(task, result);
    }
    
    @Override
    public <T> List<Future<T>> invokeAll(Collection<? extends Callable<T>> tasks)
        throws InterruptedException {
        return executor.invokeAll(tasks);
    }
    
    @Override
    public <T> List<Future<T>> invokeAll(Collection<? extends Callable<T>> tasks,
                                         long timeout, TimeUnit unit)
        throws InterruptedException {
        return executor.invokeAll(tasks, timeout, unit);
    }
    
    @Override
    public <T> T invokeAny(Collection<? extends Callable<T>> tasks)
        throws InterruptedException, ExecutionException {
        return executor.invokeAny(tasks);
    }
    
    @Override
    public <T> T invokeAny(Collection<? extends Callable<T>> tasks,
                           long timeout, TimeUnit unit)
        throws InterruptedException, ExecutionException, TimeoutException {
        return executor.invokeAny(tasks, timeout, unit);
    }

}
