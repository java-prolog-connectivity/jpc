package org.jpc.internal.concurrent;

import java.util.concurrent.Executors;

public class OneThreadExecutor extends DelegatedExecutorService {

	public OneThreadExecutor() {
		super(Executors.newSingleThreadExecutor(new OneThreadFactory()));
	}

}
