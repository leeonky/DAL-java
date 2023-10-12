package com.github.leeonky.dal.type;

public interface InputValue<T> extends InputCode<T> {
    @Override
    T get();
}
