package com.github.leeonky.dal.runtime;

public interface ErrorHook {
    void handle(Object input, String code, Throwable error);
}
