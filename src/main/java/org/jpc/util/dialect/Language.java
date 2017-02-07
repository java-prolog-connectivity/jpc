package org.jpc.util.dialect;


import static java.util.Arrays.asList;
import static java.util.Collections.unmodifiableList;

import java.util.List;

public enum Language {
    PROLOG("PROLOG", "pl", "prolog", "pro"), LOGTALK("LOGTALK", "lgt", "logtalk");

    private final String name;

    private final List<String> extensions;

    Language(String name, String ...extensions) {
        this.name = name;
        this.extensions = unmodifiableList(asList(extensions));
    }

    public String getName() {
        return name;
    }

    public List<String> getExtensions() {
        return extensions;
    }
}
