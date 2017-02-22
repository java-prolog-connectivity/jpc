package org.jpc.converter.typesolver;

/**
 * Exception thrown if a type solver cannot assign a best target type to a given object in a conversion operation.
 * @author sergioc
 *
 */
public class UnrecognizedObjectException extends RuntimeException {

	private static final long serialVersionUID = 1L;

	public UnrecognizedObjectException() {
	}

	public UnrecognizedObjectException(Throwable cause) {
		super(cause);
	}

/*	public UnrecognizedObjectException(Object object) {
		super(formatMessage(object));
	}

	public UnrecognizedObjectException(Object object, Throwable cause) {
		super(formatMessage(object), cause);
	}

	private static String formatMessage(Object object) {
		return "No type defined for " + object + ".";
	}*/

}
