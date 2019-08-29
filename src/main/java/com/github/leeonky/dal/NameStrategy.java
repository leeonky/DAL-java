package com.github.leeonky.dal;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public enum NameStrategy {
    SIMPLE_NAME_WITH_PARENT() {
        @Override
        public String toName(Class<?> clazz) {
            Matcher matcher = CLASS_FULL_NAME.matcher(clazz.getName());
            if (matcher.matches())
                return matcher.group(1).replace('$', '.');
            throw new IllegalStateException();
        }
    }, SIMPLE_NAME() {
        @Override
        public String toName(Class<?> clazz) {
            return clazz.getSimpleName();
        }
    };

    private static final Pattern CLASS_FULL_NAME = Pattern.compile(".+\\.(.+)");

    public abstract String toName(Class<?> clazz);
}
