package com.github.leeonky.dal;

import com.github.leeonky.dal.util.DataObject;

import java.util.function.Function;

@FunctionalInterface
public interface ConstructorViaSchema extends Function<DataObject, Object> {
}
