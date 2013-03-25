package org.jpc.query;

import org.jpc.JpcException;

public class InvalidCursorStateException extends JpcException {

	public InvalidCursorStateException() {}
	
	public InvalidCursorStateException(String message) {super(message);}

}
