package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.IndexedElement;

import java.util.Iterator;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import static java.lang.String.format;

public abstract class DALCollection<E> implements Iterable<IndexedElement<E>> {
    public abstract int size();

    public E getByIndex(int index) {
        try {
            if (index < 0) {
                return requireLimitedCollection("Not support negative index in infinite collection")
                        .getByPosition(size() + index);
            }
            return getByPosition(index - firstIndex());
        } catch (IndexOutOfBoundsException e) {
            throw new IndexOutOfBoundsException(format("Index out of bounds (%d), first index is: %d",
                    index, firstIndex()));
        }
    }

    public int firstIndex() {
        return 0;
    }

    public DALCollection<E> requireLimitedCollection(String message) {
        if (infinite())
            throw new InfiniteCollectionException(message);
        return this;
    }

    protected abstract E getByPosition(int position);

    public List<E> collect() {
        return requireLimitedCollection("Not supported for infinite collection").stream()
                .map(IndexedElement::value).collect(Collectors.toList());
    }

    //    TODO tobe abstract
    public DALCollection<E> filter(Predicate<E> predicate) {
        return this;
    }

    public Stream<E> values() {
        return stream().map(IndexedElement::value);
    }

    public Stream<Integer> indexes() {
        return stream().map(IndexedElement::index);
    }

    public <R> DALCollection<R> map(IndexedElement.Mapper<? super E, ? extends R> mapper) {
        return new DALCollection<R>() {
            @Override
            public int size() {
                return DALCollection.this.size();
            }

            @Override
            public int firstIndex() {
                return DALCollection.this.firstIndex();
            }

            @Override
            protected R getByPosition(int position) {
                return mapper.apply(position + firstIndex(), DALCollection.this.getByPosition(position));
            }

            @Override
            public Iterator<IndexedElement<R>> iterator() {
                return new Iterator<IndexedElement<R>>() {
                    final Iterator<IndexedElement<E>> iterator = (DALCollection.this).iterator();

                    @Override
                    public boolean hasNext() {
                        return iterator.hasNext();
                    }

                    @Override
                    public IndexedElement<R> next() {
                        return iterator.next().map(mapper);
                    }
                };
            }

            @Override
            public boolean infinite() {
                return DALCollection.this.infinite();
            }
        };
    }

    public Stream<IndexedElement<E>> stream() {
        return StreamSupport.stream(spliterator(), false);
    }

    public boolean infinite() {
        return false;
    }

    public DALCollection<Object> limit(Integer size) {
        return new CollectionDALCollection<Object>(values().limit(size).collect(Collectors.toList())) {
            @Override
            public int firstIndex() {
                return DALCollection.this.firstIndex();
            }
        };
    }

    public static class Decorated<E> extends DALCollection<E> {

        private final DALCollection<E> origin;

        public Decorated(DALCollection<E> origin) {
            this.origin = origin;
        }

        @Override
        public int size() {
            return origin.size();
        }

        @Override
        protected E getByPosition(int position) {
            return origin.getByPosition(position);
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
        public boolean infinite() {
            return origin.infinite();
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
        public Decorated<E> requireLimitedCollection(String message) {
            origin.requireLimitedCollection(message);
            return this;
        }
    }
}
