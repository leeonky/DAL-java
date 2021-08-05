package com.github.leeonky.dal.util;

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
}
