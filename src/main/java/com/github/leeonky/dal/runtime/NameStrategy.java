package com.github.leeonky.dal.runtime;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public interface NameStrategy {
    NameStrategy SIMPLE_NAME = Class::getSimpleName;
    NameStrategy SIMPLE_NAME_WITH_PARENT = clazz -> {
        Matcher matcher = Pattern.compile(".+\\.(.+)").matcher(clazz.getName());
        return (matcher.matches() ? matcher.group(1) : clazz.getName()).replace('$', '.');
    };

    String toName(Class<?> clazz);
}
