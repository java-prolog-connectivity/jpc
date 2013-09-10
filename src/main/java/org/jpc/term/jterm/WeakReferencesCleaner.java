package org.jpc.term.jterm;


public class WeakReferencesCleaner extends Thread {

	private RefManager refManager;
	
	public WeakReferencesCleaner(RefManager refManager, int priority) {
		this.refManager = refManager;
		this.setDaemon(true);
		this.setPriority(priority);
	}

	@Override
	public void run() {
		while(true) {
			try {
				JRef jRef = (JRef) refManager.getReferenceQueue().remove();
				RefId refId = jRef.getRefId();
				refManager.remove(refId);
			} catch (InterruptedException e) {}
		}
	}

}
