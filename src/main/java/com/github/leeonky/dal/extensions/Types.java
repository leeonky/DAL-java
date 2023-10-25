package com.github.leeonky.dal.extensions;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.runtime.*;

import java.util.Collection;
import java.util.Map;
import java.util.stream.Stream;

import static com.github.leeonky.dal.runtime.ListAccessor.changeFirstIndex;
import static com.github.leeonky.dal.runtime.Order.BUILD_IN;

@Order(BUILD_IN)
public class Types implements Extension {

    @Override
    public void extend(DAL dal) {
        RuntimeContextBuilder builder = dal.getRuntimeContextBuilder();
        builder.registerListAccessor(AutoMappingList.class, changeFirstIndex(AutoMappingList::firstIndex))
                .registerPropertyAccessor(Map.class, new MapPropertyAccessor())
                .registerPropertyAccessor(AutoMappingList.class, new AutoMappingListPropertyAccessor())
                .registerPropertyAccessor(CurryingMethod.class, new CurryingMethodPropertyAccessor(builder))
                .registerDataListFactory(Iterable.class, IterableDataList::new)
                .registerDataListFactory(Collection.class, CollectionDataList::new)
                .registerDataListFactory(Stream.class, (stream, comparator) ->
                        new IterableDataList<Object>(stream::iterator, comparator));
    }
}
