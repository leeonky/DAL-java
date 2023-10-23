package com.github.leeonky.dal.extensions;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.runtime.*;

import java.util.Map;
import java.util.stream.Stream;

import static com.github.leeonky.dal.runtime.ListAccessor.changeFirstIndex;
import static com.github.leeonky.dal.runtime.Order.BUILD_IN;

@Order(BUILD_IN)
public class Types implements Extension {

    @Override
    public void extend(DAL dal) {
        RuntimeContextBuilder builder = dal.getRuntimeContextBuilder();
        builder.registerListAccessor(Iterable.class, iterable -> iterable)
                .registerListAccessor(Stream.class, stream -> stream::iterator)
                .registerListAccessor(AutoMappingList.class, changeFirstIndex(AutoMappingList::firstIndex))
                .registerPropertyAccessor(Map.class, new MapPropertyAccessor())
                .registerPropertyAccessor(AutoMappingList.class, new AutoMappingListPropertyAccessor())
                .registerPropertyAccessor(CurryingMethod.class, new CurryingMethodPropertyAccessor(builder))
        ;
    }
}
