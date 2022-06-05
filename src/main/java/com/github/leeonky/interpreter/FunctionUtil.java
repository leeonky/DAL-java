package com.github.leeonky.interpreter;

import java.util.Objects;
import java.util.Optional;
import java.util.function.BinaryOperator;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Stream;

public class FunctionUtil {

    @SuppressWarnings("unchecked")
    public static <T> Predicate<T> not(Predicate<? extends T> t) {
        Objects.requireNonNull(t);
        return (Predicate<T>) t.negate();
    }

    @SuppressWarnings("unchecked")
    @SafeVarargs
    public static <T> Optional<T> oneOf(Supplier<Optional<? extends T>>... optionals) {
        return (Optional<T>) Stream.of(optionals).map(Supplier::get).filter(Optional::isPresent)
                .findFirst().orElse(Optional.empty());
    }

    public static <T> BinaryOperator<T> notAllowParallelReduce() {
        return (o1, o2) -> {
            throw new IllegalStateException("Not allow parallel here!");
        };
    }
}
