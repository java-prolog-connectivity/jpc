package org.jpc.visitor;

import java.util.Map;

import org.jpc.term.Term;
import org.jpc.term.TermConvertable;
import org.jpc.term.Variable;

public class ReplaceVariableVisitor extends JpcAdapterVisitor {
	private Map<String, TermConvertable> map;
	
	public ReplaceVariableVisitor(Map<String, TermConvertable> map) {
		this(new JpcWriterVisitor(), map);
	}
	
	public ReplaceVariableVisitor(JpcStreamingVisitor adaptee, Map<String, TermConvertable> map) {
		super(adaptee);
		this.map = map;
	}
	
	@Override
	public void visitVariable(Variable var) {
		TermConvertable termConvertable = map.get(var.name);
		if(termConvertable != null) {
			Term transformedTerm = termConvertable.asTerm();
			transformedTerm.accept(adaptee);
		}
		else
			super.visitVariable(var);
	}

}
