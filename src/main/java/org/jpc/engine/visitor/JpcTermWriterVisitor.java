package org.jpc.engine.visitor;

import java.util.Deque;
import java.util.LinkedList;
import java.util.List;

import org.jpc.term.Atom;
import org.jpc.term.FloatTerm;
import org.jpc.term.IntegerTerm;
import org.jpc.term.Term;
import org.jpc.term.Variable;
import org.jpc.util.JpcTermBuilder;
import org.jpc.util.TermBuilder;

public class JpcTermWriterVisitor implements JpcStreamingVisitor {

	Deque<TermBuilder<Term>> queue = new LinkedList<>();
	
	public List<Term> terms() {
		return TermBuilder.asTerms(queue.descendingIterator());
	}
	
	@Override
	public void visitInteger(IntegerTerm term) {
		queue.push(new JpcTermBuilder(term));
	}

	@Override
	public void visitFloat(FloatTerm term) {
		queue.push(new JpcTermBuilder(term));
	}

	@Override
	public void visitVariable(Variable term) {
		queue.push(new JpcTermBuilder(term));
	}
	
	@Override
	public void visitAtom(Atom term) {
		queue.push(new JpcTermBuilder(term));
	}

	@Override
	public void visitCompound() {
		queue.push(new JpcTermBuilder());
	}

	@Override
	public void endVisitCompound() {
	}

	@Override
	public void visitCompoundName() {
	}
	
	@Override
	public void endVisitCompoundName() {
		TermBuilder compoundNameBuilder = queue.pop();
		TermBuilder compoundBuilder = queue.peek();
		compoundBuilder.setFunctor(compoundNameBuilder.asTerm());
	}

	@Override
	public void visitCompoundArg() {
	}
	
	@Override
	public void endVisitCompoundArg() {
		TermBuilder compoundArgBuilder = queue.pop();
		TermBuilder compoundBuilder = queue.peek();
		compoundBuilder.addArg(compoundArgBuilder.asTerm());
	}

	
}
