package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.IndexedElement;

import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import static java.lang.String.format;

public abstract class DALCollectionBase<E> implements DALCollection<E> {

    @Override
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

    @Override
    public int firstIndex() {
        return 0;
    }

    @Override
    public DALCollectionBase<E> requireLimitedCollection(String message) {
        if (infinite())
            throw new InfiniteCollectionException(message);
        return this;
    }

    protected abstract E getByPosition(int position);

    @Override
    public List<E> collect() {
        return requireLimitedCollection("Not supported for infinite collection").stream()
                .map(IndexedElement::value).collect(Collectors.toList());
    }

    @Override
    public Stream<E> values() {
        return stream().map(IndexedElement::value);
    }

    @Override
    public Stream<Integer> indexes() {
        return stream().map(IndexedElement::index);
    }

    @Override
    public <R> DALCollectionBase<R> map(IndexedElement.Mapper<? super E, ? extends R> mapper) {
        return new DALCollectionBase<R>() {
            @Override
            public int size() {
                return DALCollectionBase.this.size();
            }

            @Override
            public int firstIndex() {
                return DALCollectionBase.this.firstIndex();
            }

            @Override
            protected R getByPosition(int position) {
                return mapper.apply(position + firstIndex(), DALCollectionBase.this.getByPosition(position));
            }

            @Override
            public Iterator<IndexedElement<R>> iterator() {
                return new Iterator<IndexedElement<R>>() {
                    final Iterator<IndexedElement<E>> iterator = (DALCollectionBase.this).iterator();

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
                return DALCollectionBase.this.infinite();
            }
        };
    }

    @Override
    public Stream<IndexedElement<E>> stream() {
        return StreamSupport.stream(spliterator(), false);
    }

    @Override
    public boolean infinite() {
        return false;
    }

    @Override
    public DALCollection<Object> limit(int size) {
        return new CollectionDALCollection<Object>(values().limit(size).collect(Collectors.toList())) {
            @Override
            public int firstIndex() {
                return DALCollectionBase.this.firstIndex();
            }
        };
    }
}
