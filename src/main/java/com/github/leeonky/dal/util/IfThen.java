package com.github.leeonky.dal.util;

import java.util.function.Supplier;

public class IfThen {
    private final boolean bool;

    private IfThen(boolean bool) {
        this.bool = bool;
    }

    public static IfThen when(boolean bool) {
        return new IfThen(bool);
    }

    public boolean then(Runnable runnable) {
        if (bool)
            runnable.run();
        return bool;
    }

    public <T> T thenReturn(Supplier<T> supplier) {
        if (bool)
            return supplier.get();
        return null;
    }
}
