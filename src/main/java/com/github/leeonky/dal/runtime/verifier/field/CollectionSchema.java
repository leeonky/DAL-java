package com.github.leeonky.dal.runtime.verifier.field;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.verifier.field.ContainerSchemaTrait.ContainerSchema;
import com.github.leeonky.util.BeanClass;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import static com.github.leeonky.util.CollectionHelper.toStream;
import static java.util.stream.Collectors.toList;
import static java.util.stream.IntStream.range;

class CollectionSchema extends ContainerSchema {
    public CollectionSchema(String subPrefix, BeanClass<?> type, Object expect, Data actual) {
        super(subPrefix, type, expect, actual, "%s[%d]");
    }

    @Override
    public BeanClass<?> getElementType(BeanClass<?> type, String subPrefix) {
        return type.getElementType();
    }

    @Override
    public List<?> getActualProperties(Data actual) {
        return range(0, actual.getListSize()).boxed().collect(toList());
    }

    static class CollectionContentSchema extends CollectionSchema implements ContainerSizeSchemaTrait {
        public CollectionContentSchema(String subPrefix, BeanClass<?> type, Object expect, Data actual) {
            super(subPrefix, type, expect, actual);
        }

        @Override
        public Map<?, Object> getExpectValues(Object expect) {
            return new LinkedHashMap<Integer, Object>() {{
                AtomicInteger index = new AtomicInteger(0);
                toStream(expect).forEach(e -> put(index.getAndIncrement(), e));
            }};
        }
    }
}
