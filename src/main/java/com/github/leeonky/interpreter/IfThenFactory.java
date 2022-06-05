package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.function.Supplier;

import static java.util.Optional.empty;
import static java.util.Optional.ofNullable;

public class IfThenFactory {
    public static IfTrue when(boolean flag) {
        return new IfTrue(flag);
    }

    public static class IfTrue {
        private final boolean flag;

        private IfTrue(boolean flag) {
            this.flag = flag;
        }

        public <T> Optional<T> optional(Supplier<T> supplier) {
            return flag ? ofNullable(supplier.get()) : empty();
        }
    }
}
