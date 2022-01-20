package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.function.Supplier;

import static java.util.Optional.empty;
import static java.util.Optional.of;

public class IfThenFactory {
    public static IfTrue when(boolean bool) {
        return new IfTrue(bool);
    }

    public static class IfTrue {
        private final boolean bool;

        private IfTrue(boolean bool) {
            this.bool = bool;
        }

        public <T> Optional<T> optional(Supplier<T> supplier) {
            if (bool)
                return of(supplier.get());
            return empty();
        }
    }
}
