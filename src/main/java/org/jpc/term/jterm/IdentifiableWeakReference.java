package org.jpc.term.jterm;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;

public class IdentifiableWeakReference<ID_TYPE, REF_TYPE> extends WeakReference<REF_TYPE> {

	private ID_TYPE refId;
	
	public IdentifiableWeakReference(ID_TYPE refId, REF_TYPE referent, ReferenceQueue<REF_TYPE> referenceQueue) {
		super(referent, referenceQueue);
		this.refId = refId;
	}

	public ID_TYPE getRefId() {
		return refId;
	}
	
	@Override
	public int hashCode() {
		return refId.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if(obj instanceof IdentifiableWeakReference) {
			return refId.equals(((IdentifiableWeakReference)obj).getRefId());
		}
		return false;
	}
}
