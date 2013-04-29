package org.jpc.converter.instantiation;

import java.lang.reflect.Type;

public interface InstanceCreator {

	public <T> T instantiate(Type type);
	
	public boolean canInstantiate(Type type);
	
}
