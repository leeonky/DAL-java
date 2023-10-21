package com.github.leeonky.dal;

import java.util.Iterator;
import java.util.function.BiConsumer;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public class Zipped<L, R> implements Iterable<Zipped<L, R>.ZippedEntry> {
    private final Iterator<L> left;
    private final Iterator<R> right;

    private Zipped(Iterator<L> left, Iterator<R> right) {
        this.left = left;
        this.right = right;
    }

    public static <L, R> Zipped<L, R> zip(Iterable<L> left, Iterable<R> right) {
        return new Zipped<>(left.iterator(), right.iterator());
    }

    public static <L, R> Zipped<L, R> zip(Stream<L> left, Iterable<R> right) {
        return new Zipped<>(left.iterator(), right.iterator());
    }

    public static <L, R> Zipped<L, R> zip(Stream<L> left, Stream<R> right) {
        return new Zipped<>(left.iterator(), right.iterator());
    }

    public static <L, R> Zipped<L, R> zip(Iterable<L> left, Stream<R> right) {
        return new Zipped<>(left.iterator(), right.iterator());
    }

    private int index = 0;

    @Override
    public Iterator<ZippedEntry> iterator() {
        return new Iterator<ZippedEntry>() {
            @Override
            public boolean hasNext() {
                return hasLeft() && hasRight();
            }

            @Override
            public ZippedEntry next() {
                return new ZippedEntry(index++, left.next(), right.next());
            }
        };
    }

    public Zipped<L, R> forEachElement(BiConsumer<L, R> consumer) {
        forEach(e -> consumer.accept(e.left(), e.right()));
        return this;
    }

    public boolean hasLeft() {
        return left.hasNext();
    }

    public boolean hasRight() {
        return right.hasNext();
    }

    public Iterable<L> left() {
        return () -> left;
    }

    public Iterable<R> right() {
        return () -> right;
    }

    public Zipped<L, R> forEachElementWithIndex(IndexedBiConsumer<L, R> consumer) {
        forEach(e -> consumer.accept(e.index(), e.left(), e.right()));
        return this;
    }

    public Stream<ZippedEntry> stream() {
        return StreamSupport.stream(spliterator(), false);
    }

    public int index() {
        return index;
    }

    @FunctionalInterface
    public interface IndexedBiConsumer<L, R> {
        void accept(int index, L left, R right);
    }

    public class ZippedEntry {
        private final int index;
        private final L left;
        private final R right;

        public ZippedEntry(int index, L left, R right) {
            this.left = left;
            this.right = right;
            this.index = index;
        }

        public int index() {
            return index;
        }

        public L left() {
            return left;
        }

        public R right() {
            return right;
        }
    }
}
