package com.github.leeonky.dal.runtime;

import java.util.function.BiFunction;

@FunctionalInterface
public interface ConstructorViaSchema extends BiFunction<Data, RuntimeContextBuilder.DALRuntimeContext, Object> {
}
