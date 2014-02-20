package org.jpc.engine.embedded;

import java.util.List;

import org.jpc.term.Term;

/**
 * A rule in the embedded JPC engine.
 * @author sergioc
 *
 */
//Work in progress.
//Rules are currently not supported in the JPC embedded engine. The current prototype only works with facts since that is enough to support the current requirements.
public class Rule extends Clause {

	private final List<Term> body;
	
	public Rule(Term head, List<Term> body, int id) {
		super(head, id);
		this.body = body;
	}

}
