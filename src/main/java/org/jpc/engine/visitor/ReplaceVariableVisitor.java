package org.jpc.engine.visitor;

import java.util.Map;

import org.jpc.term.Term;
import org.jpc.term.TermAdaptable;
import org.jpc.term.Variable;

public class ReplaceVariableVisitor extends JpcTermAdapterVisitor {
	private Map<String, TermAdaptable> map;
	
	public ReplaceVariableVisitor(Map<String, TermAdaptable> map) {
		this(new JpcTermWriterVisitor(), map);
	}
	
	public ReplaceVariableVisitor(TermStreamingVisitor adaptee, Map<String, TermAdaptable> map) {
		super(adaptee);
		this.map = map;
	}
	
	@Override
	public void visitVariable(Variable var) {
		TermAdaptable termAdaptable = map.get(var.name);
		if(termAdaptable != null) {
			Term transformedTerm = termAdaptable.asTerm();
			transformedTerm.accept(adaptee);
		}
		else
			super.visitVariable(var);
	}

}
