package com.github.leeonky.dal;

import java.util.function.BiFunction;

@FunctionalInterface
public interface Constructor extends BiFunction<Object, RuntimeContext, Object> {
}
