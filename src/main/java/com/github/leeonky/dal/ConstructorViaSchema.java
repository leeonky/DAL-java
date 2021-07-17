package com.github.leeonky.dal;

import java.util.function.BiFunction;

@FunctionalInterface
public interface ConstructorViaSchema extends BiFunction<Object, RuntimeContext, Object> {
}
