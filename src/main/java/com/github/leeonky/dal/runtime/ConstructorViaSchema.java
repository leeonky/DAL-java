package com.github.leeonky.dal.runtime;

import java.util.function.Function;

@FunctionalInterface
public interface ConstructorViaSchema extends Function<Data, Object> {
}
