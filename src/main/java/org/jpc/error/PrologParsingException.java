package org.jpc.error;

import org.jpc.JpcException;

/**
 * Exception thrown when attempting to parse non-conforming Prolog code.
 * Unfortunately, there is not (or I do not know) a simple mechanism for converting atoms to terms and reporting syntax errors
 *  that works reasonably consistently across different Prolog implementations.
 * For example, assuming the operator :: is not defined, this query in SWI (or YAP):
 * 	 ?- catch(atom_to_term('x::x', T, V), E, true).
 * Results in:
 *	 E = error(syntax_error(operator_expected), string("x::x . ", 1)).
 * So an appropriate syntax error is reported. As with common Prolog exceptions, this Prolog error is mapped to a java SyntaxError exception class.
 * Although at first glance it seems like a good idea to throw SyntaxError exceptions wrapping a Prolog error term, this is not easy to port to other Prolog implementations.
 * For example, the XSB read_atom_to_term/3 is relatively equivalent to the SWI atom_to_term/3 predicate.
 * However, the following XSB query will fail:
 *   ?- catch(read_atom_to_term('x::x',T,V), E, true).
 * Then, instead of relying on the underlying Prolog implementation to report correctly a syntax error, the framework will throw an instance of this class encapsulating, if available, a SyntaxError exception.
 * @author sergioc
 *
 */
public class PrologParsingException extends JpcException {

	private String source;

	public PrologParsingException(String source) {
		this.source = source;
	}

	public PrologParsingException(String source, Exception cause) {
		super(cause);
		this.source = source;
	}

	public String getSource() {
		return source;
	}
	
	@Override
	public String getMessage() {
		return "Parsing exception.";
	}

}
