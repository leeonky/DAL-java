package com.github.leeonky.dal.util;

import java.util.function.Function;
import java.util.function.Supplier;

public class IfThenFactory {
    public static IfTrue when(boolean bool) {
        return new IfTrue(bool);
    }

    public static <T, R> IfNonNull<T, R> whenNonNull(T value) {
        return new IfNonNull<>(value);
    }

    public static class IfTrue {
        private final boolean bool;

        private IfTrue(boolean bool) {
            this.bool = bool;
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

    public static class IfNonNull<T, R> {
        private final T value;

        public IfNonNull(T value) {
            this.value = value;
        }

        public Else canReturn(Function<T, R> function) {
            return new Else(function);
        }

        public class Else {
            private final Function<T, R> nonNull;

            public Else(Function<T, R> nonNull) {
                this.nonNull = nonNull;
            }

            public R orElse(Runnable run) {
                if (value != null)
                    return nonNull.apply(value);
                run.run();
                return null;
            }
        }
    }
}
