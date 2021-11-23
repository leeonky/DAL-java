package com.github.leeonky.dal.runtime;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

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

    public static <T> List<List<T>> transpose(List<List<T>> list) {
        return transpose(list.stream()).collect(Collectors.toList());
    }

    public static <T> Stream<List<T>> transpose(Stream<List<T>> list) {
        return new LinkedHashMap<Integer, List<T>>() {{
            list.forEach(colCells -> eachWithIndex(colCells.stream(),
                    (i, colCell) -> computeIfAbsent(i, key -> new ArrayList<>()).add(colCell)));
        }}.values().stream();
    }

    public static <A, B, C> Stream<C> zip(Stream<A> streamA, Stream<B> streamB, BiFunction<A, B, C> zipper) {
        Iterator<A> iteratorA = streamA.iterator();
        Iterator<B> iteratorB = streamB.iterator();
        Iterator<C> iteratorC = new Iterator<C>() {
            @Override
            public boolean hasNext() {
                return iteratorA.hasNext() && iteratorB.hasNext();
            }

            @Override
            public C next() {
                return zipper.apply(iteratorA.next(), iteratorB.next());
            }
        };
        return StreamSupport.stream(((Iterable<C>) () -> iteratorC).spliterator(),
                streamA.isParallel() || streamB.isParallel());
    }

    public static <T> void eachWithIndex(Stream<T> stream, BiConsumer<Integer, T> consumer) {
        AtomicInteger atomicInteger = new AtomicInteger(0);
        stream.forEach(element -> consumer.accept(atomicInteger.getAndIncrement(), element));
    }

    public static <T, R> Stream<R> mapWithIndex(Stream<T> stream, BiFunction<Integer, T, R> biFunction) {
        AtomicInteger atomicInteger = new AtomicInteger(0);
        return stream.map(element -> biFunction.apply(atomicInteger.getAndIncrement(), element));
    }

    public static <T> BinaryOperator<T> notAllowParallelReduce() {
        return (o1, o2) -> {
            throw new IllegalStateException("Not allow parallel here!");
        };
    }
}
