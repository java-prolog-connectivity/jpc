package org.jpc.engine.dialect;


import static java.util.Collections.emptyMap;
import static java.util.Collections.unmodifiableMap;

import java.util.Map;

import com.google.common.collect.ImmutableMap;

public enum Dialect {

    JPC,
    SWI(ImmutableMap.<DialectProperties, Object>builder()
            .put(DialectProperties.CONS_FUNCTOR, "[|]").build()),
    YAP,
    XSB;

    public static Dialect fromDialectFlag(String dialectFlag) {
        return Dialect.valueOf(dialectFlag.toUpperCase());
    }

    private final Map<DialectProperties, Object> properties;

    Dialect() {
        this.properties = emptyMap();
    }

    Dialect(Map<DialectProperties, Object> properties) {
        this.properties = unmodifiableMap(properties);
    }

    public <T> T getProperty(DialectProperties property) {
        return (T) properties.get(property);
    }

    /**
     *
     * @return the dialect Prolog flag associated with engines of this type.
     */
    public String asDialectFlag() {
        return this.name().toLowerCase();
    }

}
