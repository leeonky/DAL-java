package com.github.leeonky.dal.runtime;

import com.github.leeonky.util.CollectionHelper;

import java.util.Comparator;
import java.util.stream.Collectors;

public class JavaArrayDALCollectionFactory extends CollectionDALCollection<Object> {
    public JavaArrayDALCollectionFactory(Object array, Comparator<Object> comparator) {
        super(CollectionHelper.toStream(array).collect(Collectors.toList()), comparator);
    }
}
