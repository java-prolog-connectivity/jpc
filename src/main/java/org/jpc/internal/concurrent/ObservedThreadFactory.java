package org.jpc.internal.concurrent;

import java.util.Collection;
import java.util.concurrent.ThreadFactory;

import org.jpc.internal.collections.CollectionsUtil;


public class ObservedThreadFactory implements ThreadFactory {

	private Collection<ThreadFactoryObserver> observers;
	
	public ObservedThreadFactory() {
		observers = CollectionsUtil.createWeakSet();
	}

	public void addObserver(ThreadFactoryObserver observer) {
		observers.add(observer);
	}

	@Override
	public Thread newThread(Runnable r) {
		Thread thread = new Thread(r);
		notifyObservers();
		return thread;
	}

	private void notifyObservers() {
		for(ThreadFactoryObserver observer : observers) {
			observer.onNewThreadCreated();
		}
	}

}
