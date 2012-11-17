package org.jpc.term;

import java.util.List;
import java.util.Map;

import org.jpc.engine.visitor.AbstractJplVisitor;

public class TermWrapper implements Term {

	private Term wrappedTerm;
	
	public Term asTerm() {
		return this;
	}

	public Term getWrappedTerm() {
		return wrappedTerm;
	}

	public Term arg(int i) {
		return wrappedTerm.arg(i);
	}

	public List<Term> args() {
		return wrappedTerm.args();
	}

	public int arity() {
		return wrappedTerm.arity();
	}

	public boolean hasFunctor(String nameTermObject, int arity) {
		return wrappedTerm.hasFunctor(nameTermObject, arity);
	}

	public boolean hasFunctor(TermConvertable nameTermObject, int arity) {
		return wrappedTerm.hasFunctor(nameTermObject, arity);
	}

	public boolean isListTerm() {
		return wrappedTerm.isListTerm();
	}

	public ListTerm asListTerm() {
		return wrappedTerm.asListTerm();
	}

	public boolean isBound() {
		return wrappedTerm.isBound();
	}

	public void accept(AbstractJplVisitor termVisitor) {
		wrappedTerm.accept(termVisitor);
	}

	public int listLength() {
		return wrappedTerm.listLength();
	}

	public boolean termEquals(TermConvertable o) {
		return wrappedTerm.termEquals(o);
	}

	public Term replaceVariables(Map<String, TermConvertable> map) {
		return wrappedTerm.replaceVariables(map);
	}

	public Term changeVariablesNames(Map<String, String> map) {
		return wrappedTerm.changeVariablesNames(map);
	}

	public List<String> getVariablesNames() {
		return wrappedTerm.getVariablesNames();
	}

	public boolean hasVariable(String variableName) {
		return wrappedTerm.hasVariable(variableName);
	}

	public List<String> nonAnonymousVariablesNames() {
		return wrappedTerm.nonAnonymousVariablesNames();
	}

	public boolean hasFunctor(boolean nameTermObject, int arity) {
		return wrappedTerm.hasFunctor(nameTermObject, arity);
	}

	public boolean hasFunctor(double nameTermObject, int arity) {
		return wrappedTerm.hasFunctor(nameTermObject, arity);
	}

	public boolean hasFunctor(long nameTermObject, int arity) {
		return wrappedTerm.hasFunctor(nameTermObject, arity);
	}

	public TermWrapper(Term wrappedTerm) {
		this.wrappedTerm = wrappedTerm;
	}
	
}
