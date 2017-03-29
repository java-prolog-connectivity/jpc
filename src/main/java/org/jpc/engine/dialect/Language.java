package org.jpc.engine.dialect;


import static com.google.common.base.Preconditions.checkArgument;
import static java.util.Arrays.asList;
import static java.util.Collections.unmodifiableList;

import java.util.List;

public enum Language {
    PROLOG("PROLOG", "pl", asList("pl", "prolog", "pro")), LOGTALK("LOGTALK", "lgt", asList("lgt", "logtalk"));

    private final String name;
    private final String defaultExtension;
    private final List<String> extensions;

    Language(String name, String defaultExtension, List<String> extensions) {
        checkArgument(extensions.contains(defaultExtension));
        this.name = name;
        this.defaultExtension = defaultExtension;
        this.extensions = unmodifiableList((extensions));
    }

    public String getName() {
        return name;
    }

    public String getDefaultExtension() {
        return defaultExtension;
    }

    public List<String> getExtensions() {
        return extensions;
    }
}
