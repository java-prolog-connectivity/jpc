package org.jpc.converter;

import java.lang.reflect.Type;

import org.minitoolbox.reflection.wrappertype.TypeWrapper;

import com.google.common.reflect.TypeToken;

public abstract class JpcConverter<S,T> {

	protected Type sourceType;
	protected Type targetType;
	
	public JpcConverter() {
		sourceType = new TypeToken<S>(getClass()){}.getType();
		targetType = new TypeToken<T>(getClass()){}.getType();
	}

	public Type getSourceType() {
		return sourceType;
	}
	
	public Type getTargetType() {
		return targetType;
	}

	public boolean targetTypeIsEquals(Type type) {
		return TypeWrapper.wrap(targetType).equals(type);
	}
	
	public boolean targetTypeIsAssignableTo(Type type) {
		return TypeWrapper.wrap(type).isWeakAssignableFrom(targetType);
	}

	public boolean targetTypeIsAssignableFrom(Type type) {
		return TypeWrapper.wrap(targetType).isWeakAssignableFrom(type);
	}
	
	public boolean sourceTypeIsEquals(Type type) {
		return TypeWrapper.wrap(sourceType).equals(type);
	}
	
	public boolean sourceTypeIsAssignableTo(Type type) {
		return TypeWrapper.wrap(sourceType).isWeakAssignableFrom(type);
	}

}
