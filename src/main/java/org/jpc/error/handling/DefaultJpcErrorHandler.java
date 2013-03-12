package org.jpc.error.handling;

public class DefaultJpcErrorHandler extends RootErrorHandlerManager {

	public DefaultJpcErrorHandler() {
		register(new JpcConverterErrorHandler());
	}
}
