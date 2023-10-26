package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.IndexedElement;

import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import static java.lang.String.format;

public abstract class DALCollection<E> implements Iterable<IndexedElement<E>> {
    public abstract int size();

    public E getByIndex(int index) {
        try {
            if (index < 0)
                return getByPosition(size() + index);
            return getByPosition(index - firstIndex());
        } catch (IndexOutOfBoundsException e) {
            throw new IndexOutOfBoundsException(format("Index out of bounds (%d), first index is: %d",
                    index, firstIndex()));
        }
    }

    protected int firstIndex() {
        return 0;
    }

    protected abstract E getByPosition(int position);

    public List<E> collect() {
        return stream().map(IndexedElement::value).collect(Collectors.toList());
    }

    public <R> DALCollection<R> map(IndexedElement.Mapper<? super E, ? extends R> mapper) {
        return new DALCollection<R>() {
            @Override
            public int size() {
                return DALCollection.this.size();
            }

            @Override
            protected int firstIndex() {
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
        };
    }

    public Stream<IndexedElement<E>> stream() {
        return StreamSupport.stream(spliterator(), false);
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
        protected int firstIndex() {
            return origin.firstIndex();
        }

        @Override
        public List<E> collect() {
            return origin.collect();
        }
    }
}
