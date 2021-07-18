package com.github.leeonky.dal;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public interface NameStrategy {
    NameStrategy SIMPLE_NAME = Class::getSimpleName;
    NameStrategy SIMPLE_NAME_WITH_PARENT = clazz -> {
        Matcher matcher = Pattern.compile(".+\\.(.+)").matcher(clazz.getName());
        if (matcher.matches())
            return matcher.group(1).replace('$', '.');
        throw new IllegalStateException();
    };

    String toName(Class<?> clazz);
}
