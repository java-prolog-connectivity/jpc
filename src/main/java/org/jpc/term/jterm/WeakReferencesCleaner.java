package org.jpc.term.jterm;


public class WeakReferencesCleaner extends Thread {

	private JRefManager refManager;
	
	public WeakReferencesCleaner(JRefManager refManager, int priority) {
		this.refManager = refManager;
		this.setDaemon(true);
		this.setPriority(priority);
	}

	@Override
	public void run() {
		while(true) {
			try {
				JRef<?> jRef = (JRef<?>) refManager.getReferenceQueue().remove();
				JRefId refId = jRef.getRefId();
				refManager.remove(refId);
			} catch (InterruptedException e) {}
		}
	}

}
