package com.github.leeonky.dal.extensions;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.runtime.AutoMappingList;
import com.github.leeonky.dal.runtime.CurryingMethod;
import com.github.leeonky.dal.runtime.Extension;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.Map;
import java.util.stream.Stream;

import static com.github.leeonky.dal.runtime.ListAccessor.changeFirstIndex;

public class Types implements Extension {
    @Override
    public void extend(DAL dal) {
        RuntimeContextBuilder builder = dal.getRuntimeContextBuilder();
        builder.registerListAccessor(Iterable.class, iterable -> iterable)
                .registerListAccessor(Stream.class, stream -> stream::iterator)
                .registerListAccessor(AutoMappingList.class, changeFirstIndex(AutoMappingList::firstIndex))
                .registerPropertyAccessor(Map.class, new MapPropertyAccessor())
                .registerPropertyAccessor(AutoMappingList.class, new AutoMappingListPropertyAccessor(builder))
                .registerPropertyAccessor(CurryingMethod.class, new CurryingMethodPropertyAccessor(builder))
        ;
    }
}
