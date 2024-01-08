package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.IndexedElement;

import java.util.Iterator;
import java.util.List;
import java.util.Spliterators;
import java.util.function.Predicate;
import java.util.stream.Stream;

public interface DALCollection<E> extends Iterable<IndexedElement<E>> {
    int size();

    E getByIndex(int index);

    int firstIndex();

    DALCollection<E> requireLimitedCollection(String message);

    List<E> collect();

    default DALCollection<E> filter(Predicate<E> predicate) {
        return new IterableDALCollection<E>(() -> Spliterators.iterator(values().filter(predicate).spliterator())) {
            @Override
            public int firstIndex() {
                return DALCollection.this.firstIndex();
            }
        };
    }

    Stream<E> values();

    Stream<Integer> indexes();

    <R> DALCollection<R> map(IndexedElement.Mapper<? super E, ? extends R> mapper);

    Stream<IndexedElement<E>> stream();

    boolean infinite();

    DALCollection<Object> limit(int size);

    class Decorated<E> implements DALCollection<E> {

        private final DALCollection<E> origin;

        public Decorated(DALCollection<E> origin) {
            this.origin = origin;
        }

        @Override
        public int size() {
            return origin.size();
        }

        @Override
        public E getByIndex(int index) {
            return origin.getByIndex(index);
        }

        @Override
        public Iterator<IndexedElement<E>> iterator() {
            return origin.iterator();
        }

        @Override
        public int firstIndex() {
            return origin.firstIndex();
        }

        @Override
        public List<E> collect() {
            return origin.collect();
        }

        @Override
        public DALCollection<E> filter(Predicate<E> predicate) {
            return origin.filter(predicate);
        }

        @Override
        public boolean infinite() {
            return origin.infinite();
        }

        @Override
        public DALCollection<Object> limit(int size) {
            return origin.limit(size);
        }

        @Override
        public Stream<E> values() {
            return origin.values();
        }

        @Override
        public Stream<Integer> indexes() {
            return origin.indexes();
        }

        @Override
        public <R> DALCollection<R> map(IndexedElement.Mapper<? super E, ? extends R> mapper) {
            return origin.map(mapper);
        }

        @Override
        public Stream<IndexedElement<E>> stream() {
            return origin.stream();
        }

        @Override
        public Decorated<E> requireLimitedCollection(String message) {
            origin.requireLimitedCollection(message);
            return this;
        }
    }
}
