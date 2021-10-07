package com.github.leeonky.dal.compiler;

import java.util.HashMap;

public class EscapeChars extends HashMap<String, Character> {
    public EscapeChars escape(String target, char c) {
        put(target, c);
        return this;
    }
}
