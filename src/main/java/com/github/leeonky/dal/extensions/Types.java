package com.github.leeonky.dal.extensions;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.runtime.*;

import java.util.Collection;
import java.util.Map;
import java.util.stream.Stream;

import static com.github.leeonky.dal.runtime.Order.BUILD_IN;

@Order(BUILD_IN)
public class Types implements Extension {

    @Override
    public void extend(DAL dal) {
        RuntimeContextBuilder builder = dal.getRuntimeContextBuilder();
        builder.registerPropertyAccessor(Map.class, new MapPropertyAccessor())
                .registerPropertyAccessor(AutoMappingList.class, new AutoMappingListPropertyAccessor())
                .registerPropertyAccessor(CurryingMethod.class, new CurryingMethodPropertyAccessor(builder))
                .registerDALCollectionFactory(Iterable.class, IterableDALCollection::new)
                .registerDALCollectionFactory(Collection.class, CollectionDALCollection::new)
                .registerDALCollectionFactory(Stream.class, (stream, comparator) ->
                        new IterableDALCollection<Object>(stream::iterator, comparator))
                .registerDALCollectionFactory(DALCollection.class, (instance, comparator) -> instance);
    }
}
