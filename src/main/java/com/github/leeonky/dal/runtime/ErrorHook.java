package com.github.leeonky.dal.runtime;

import com.github.leeonky.util.ThrowingSupplier;

public interface ErrorHook {
    void handle(ThrowingSupplier<Object> input, String code, Throwable error);
}
