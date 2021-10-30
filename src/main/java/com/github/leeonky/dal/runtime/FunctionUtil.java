package com.github.leeonky.dal.runtime;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Stream;

import static java.util.Arrays.asList;

public class FunctionUtil {
    public static <T> Predicate<T> not(Predicate<T> t) {
        return t.negate();
    }

    @SuppressWarnings("unchecked")
    @SafeVarargs
    public static <T> Optional<T> oneOf(Supplier<Optional<? extends T>>... optionals) {
        return (Optional<T>) Stream.of(optionals).map(Supplier::get).filter(Optional::isPresent)
                .findFirst().orElse(Optional.empty());
    }

    @SafeVarargs
    public static <T> T getValue(Supplier<? extends T> supplier, Supplier<? extends T>... suppliers) {
        return getValue(new ArrayList<Supplier<? extends T>>() {{
            add(supplier);
            addAll(asList(suppliers));
        }});
    }

    private static <T> T getValue(List<Supplier<? extends T>> suppliers) {
        try {
            return suppliers.get(0).get();
        } catch (RuntimeException exception) {
            if (suppliers.size() > 1) {
                return getValue(suppliers.subList(1, suppliers.size()));
            }
            throw exception;
        }
    }

    public static <T> List<T> allOptional(Supplier<Optional<T>> optional) {
        return new ArrayList<T>() {{
            for (Optional<T> t = optional.get(); t.isPresent(); t = optional.get())
                add(t.get());
        }};
    }
}
