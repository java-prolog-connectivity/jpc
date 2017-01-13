package org.jpc.internal.gc;

public class StrongReference<T> {

	private final T referent;
	
	public StrongReference(T referent) {
		this.referent = referent;
	}
	
	public T get() {
		return referent;
	}

	@Override
	public int hashCode() {
		return System.identityHashCode(referent);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		StrongReference other = (StrongReference) obj;
		if (referent == null) {
			if (other.referent != null)
				return false;
		} else if (referent != other.referent)
			return false;
		return true;
	}
	
	
}
