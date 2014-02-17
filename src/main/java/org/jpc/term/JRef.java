package org.jpc.term;

import java.lang.ref.PhantomReference;
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
import org.minitoolbox.gc.CleanableSoftReference;
import org.minitoolbox.gc.CleanableWeakReference;
import org.minitoolbox.gc.ReferenceType;
import org.minitoolbox.gc.ReferencesCleaner;

import com.google.common.base.Function;

public abstract class JRef<T> extends Term {
	
	private static ReferencesCleaner defaultJRefCleaner;

	private static synchronized ReferencesCleaner getDefaultCleaner() {
		if(defaultJRefCleaner == null) {
			defaultJRefCleaner = new ReferencesCleaner(new ReferenceQueue<Object>());
			defaultJRefCleaner.start();
		}
		return defaultJRefCleaner;
	}
	
	public static <T> JRef<T> jRef(T ref) {
		return new StrongJRef<T>(ref);
	}

	
	public static <T> JRef<T> softJRef(T ref) {
		return softJRef(ref, (ReferenceQueue)getDefaultCleaner().getReferenceQueue(), null);
	}
	
	public static <T> JRef<T> softJRef(T ref, Runnable cleaningTask) {
		return softJRef(ref, (ReferenceQueue)getDefaultCleaner().getReferenceQueue(), cleaningTask);
	}
	
	public static <T> JRef<T> softJRef(T ref, ReferenceQueue<? super T> referenceQueue) {
		return softJRef(ref, referenceQueue, null);
	}
	
	public static <T> JRef<T> softJRef(T ref, ReferenceQueue<? super T> referenceQueue, Runnable cleaningTask) {
		Reference<T> reference = new CleanableSoftReference(ref, referenceQueue, cleaningTask);
		return new WeakJRef<T>(reference);
	}
	
	
	public static <T> JRef<T> weakJRef(T ref) {
		return weakJRef(ref, (ReferenceQueue)getDefaultCleaner().getReferenceQueue(), null);
	}
	
	public static <T> JRef<T> weakJRef(T ref, Runnable cleaningTask) {
		return weakJRef(ref, (ReferenceQueue)getDefaultCleaner().getReferenceQueue(), cleaningTask);
	}
	
	public static <T> JRef<T> weakJRef(T ref, ReferenceQueue<? super T> referenceQueue) {
		return weakJRef(ref, referenceQueue, null);
	}
	
	public static <T> JRef<T> weakJRef(T ref, ReferenceQueue<? super T> referenceQueue, Runnable cleaningTask) {
		Reference<T> reference = new CleanableWeakReference(ref, referenceQueue, cleaningTask);
		return new WeakJRef<T>(reference);
	}

	
	private JRef() {}
	
	public abstract T getReferent();
	
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
	public String toEscapedString() {
		return PrologUtil.escapeString(getReferent().toString());
	}

	@Override
	public String toString(OperatorsContext o) {
		return toString();
	}

	@Override
	public String toString() {
		return getClass().getSimpleName() + "(" + getReferent().toString() + ")";
	}

	@Override
	public boolean equals(Object term) {
		return (this == term || 
				(term.getClass().equals(getClass()) && getReferent().equals(((JRef)term).getReferent())));
	}
	
	@Override
	public int hashCode() {
		return getReferent().hashCode();
	}
	
	@Override
	public void doUnification(Term term) {
		if(term instanceof AbstractVar)
			term.doUnification(this);
		else {
			if( !(term instanceof JRef && getReferent().equals(((JRef)term).getReferent())) )
				throw new NonUnifiableException(this, term); //TODO implement open-unification (?)
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
	
	@Override
	public void accept(TermVisitor termVisitor) {
		termVisitor.visitJRef(this);
	}

	
	public final static class StrongJRef<T> extends JRef<T> {
		
		private final T strongRef;
		
		public StrongJRef(T ref) {
			this.strongRef = ref;
		}
		
		@Override
		public T getReferent() {
			return strongRef;
		}
		
		@Override
		public ReferenceType getReferenceType() {
			return ReferenceType.STRONG;
		}
		
		@Override
		protected void basicRead(TermContentHandler contentHandler, Function<Term, Term> termExpander) {
			contentHandler.startJRef(getReferent());
		}
		
	}
	
	
	public final static class WeakJRef<T> extends JRef<T> {
		
		private final Reference<T> weakRef;
		
		public WeakJRef(Reference<T> weakRef) {
			this.weakRef = weakRef;
		}
		
		@Override
		public T getReferent() {
			return weakRef.get();
		}
		
		public Reference<T> getReference() {
			return weakRef;
		} 
		
		@Override
		public ReferenceType getReferenceType() {
			if(weakRef instanceof SoftReference)
				return ReferenceType.SOFT;
			else if(weakRef instanceof WeakReference)
				return ReferenceType.WEAK;
			else if(weakRef instanceof PhantomReference)
				throw new JpcException("Phantom references are not supported.");
			else
				throw new JpcException("Unrecognized reference type."); //this should never happen (just in case future versions of Java add inadvertently new reference types).
		}
		
		@Override
		protected void basicRead(TermContentHandler contentHandler, Function<Term, Term> termExpander) {
			contentHandler.startJRef(getReference());
		}
		
	}
	
}
