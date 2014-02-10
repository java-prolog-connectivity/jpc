package org.jpc.term;

import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.SoftReference;
import java.lang.ref.WeakReference;

import org.jpc.JpcException;
import org.jpc.engine.prolog.OperatorsContext;
import org.jpc.salt.TermContentHandler;
import org.jpc.term.compiled.CompilationContext;
import org.jpc.term.unification.NonUnifiableException;
import org.jpc.term.visitor.TermVisitor;
import org.jpc.util.PrologUtil;
import org.minitoolbox.reference.CleanableSoftReference;
import org.minitoolbox.reference.CleanableWeakReference;
import org.minitoolbox.reference.ReferenceType;
import org.minitoolbox.reference.ReferencesCleaner;

import com.google.common.base.Function;

public abstract class JRef<T> extends Term {
	
	private static ReferencesCleaner defaultReferencesCleaner;

	private static synchronized ReferencesCleaner getReferencesCleaner() {
		if(defaultReferencesCleaner == null) {
			defaultReferencesCleaner = new ReferencesCleaner(new ReferenceQueue<Object>());
			defaultReferencesCleaner.start();
		}
		return defaultReferencesCleaner;
	}
	
	public static <T> JRef<T> jref(T ref) {
		return new StrongJRef<T>(ref);
	}

	public static <T> JRef<T> weakJref(T ref) {
		return weakJref(ref, (ReferenceQueue)getReferencesCleaner().getReferenceQueue(), null);
	}
	
	public static <T> JRef<T> weakJref(T ref, Runnable cleaningTask) {
		return weakJref(ref, (ReferenceQueue)getReferencesCleaner().getReferenceQueue(), cleaningTask);
	}
	
	public static <T> JRef<T> weakJref(T ref, ReferenceQueue<? super T> referenceQueue, Runnable cleaningTask) {
		Reference<T> reference = new CleanableWeakReference(ref, referenceQueue, cleaningTask);
		return new WeakJRef<T>(reference);
	}
	
	public static <T> JRef<T> softJref(T ref) {
		return softJref(ref, (ReferenceQueue)getReferencesCleaner().getReferenceQueue(), null);
	}
	
	public static <T> JRef<T> softJref(T ref, Runnable cleaningTask) {
		return softJref(ref, (ReferenceQueue)getReferencesCleaner().getReferenceQueue(), cleaningTask);
	}
	
	public static <T> JRef<T> softJref(T ref, ReferenceQueue<? super T> referenceQueue, Runnable cleaningTask) {
		Reference<T> reference = new CleanableSoftReference(ref, referenceQueue, cleaningTask);
		return new WeakJRef<T>(reference);
	}
	
	JRef() {}
	
	public abstract T getRef();
	
	public abstract ReferenceType getReferenceType();
	
	@Override
	public boolean hasFunctor(Functor functor) {
		return functor.getArity() == 0 && termEquals(functor.getName());
	}

	@Override
	public boolean isGround() {
		return true;
	}
	
	@Override
	public void accept(TermVisitor termVisitor) {
		termVisitor.visitJRef(this);
	}

	@Override
	protected void basicRead(TermContentHandler contentHandler, Function<Term, Term> termExpander) {
		contentHandler.startJRef(getRef());
	}

	@Override
	public String toEscapedString() {
		return PrologUtil.escapeString(getRef().toString());
	}

	@Override
	public String toString(OperatorsContext o) {
		return toString();
	}

	@Override
	public String toString() {
		return getClass().getSimpleName() + "(" + getRef().toString() + ")";
	}

	@Override
	public boolean equals(Object term) {
		return (this == term || 
				(term.getClass().equals(getClass()) && getRef() == ((JRef)term).getRef()));
	}
	
	@Override
	public int hashCode() {
		return getRef().hashCode();
	}
	
	@Override
	public void doUnification(Term term) {
		if(term instanceof AbstractVar)
			term.doUnification(this);
		else {
			if(!equals(term))
				throw new NonUnifiableException(this, term); //TODO implement open-unification
		}
	}
	
	@Override
	public Term compile(int clauseId, CompilationContext context) {
		return this;
	}

	@Override
	public Term prepareForQuery(CompilationContext context) {
		return this;
	}

	@Override
	public Term prepareForFrame(CompilationContext context) {
		return this;
	}
	
	

	
	private static class StrongJRef<T> extends JRef<T> {
		
		private final T strongRef;
		
		StrongJRef(T ref) {
			this.strongRef = ref;
		}
		
		@Override
		public T getRef() {
			return strongRef;
		}
		
		@Override
		public ReferenceType getReferenceType() {
			return ReferenceType.STRONG;
		}
	}
	
	
	private static class WeakJRef<T> extends JRef<T> {
		
		private final Reference<T> weakRef;
		
		WeakJRef(Reference<T> weakRef) {
			this.weakRef = weakRef;
		}
		
		@Override
		public T getRef() {
			return weakRef.get();
		}
		
		@Override
		public ReferenceType getReferenceType() {
			if(weakRef instanceof SoftReference)
				return ReferenceType.SOFT;
			else if(weakRef instanceof WeakReference)
				return ReferenceType.WEAK;
			else
				throw new JpcException("Unrecognized reference type."); //this should never happen
		}
	}
	
}
