package org.jpc.converter.instantiation;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

/*
 * Utility class mapping types to instance creators
 */
public class InstantiationManager {
	List<InstanceCreator> instanceCreators;
	
	public InstantiationManager() {
		instanceCreators = new ArrayList<>();
	}

	public void register(Type type) {
		register(new SimpleInstanceCreator(type));
	}
	
	
	public void register(InstanceCreator instanceCreator) {
		instanceCreators.add(0, instanceCreator);
	}

	public <T> T instantiate(Type targetType) {
		T instantiation = null;
		for(InstanceCreator instanceCreator : instanceCreators) {
			if(instanceCreator.canInstantiate(targetType)) {
				instantiation = instanceCreator.instantiate(targetType);
				break;
			}
		}
		if(instantiation == null)
			throw new RuntimeException(new InstantiationException("Impossible to instantiate type: " + targetType));
		return instantiation;
	}

}
