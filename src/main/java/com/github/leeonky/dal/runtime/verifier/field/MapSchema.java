package com.github.leeonky.dal.runtime.verifier.field;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.verifier.field.ContainerSchemaTrait.ContainerSchema;
import com.github.leeonky.util.BeanClass;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static java.lang.String.format;

class MapSchema extends ContainerSchema {
    public MapSchema(String subPrefix, BeanClass<?> type, Object expect, Data actual) {
        super(subPrefix, type, expect, actual, "%s.%s");
    }

    @Override
    public BeanClass<?> getElementType(BeanClass<?> type, String subPrefix) {
        return type.getTypeArguments(1).orElseThrow(() ->
                new IllegalArgumentException(format("`%s` should be generic type", subPrefix)));
    }

    @Override
    public List<?> getActualProperties(Data actual) {
        return new ArrayList<>(actual.getFieldNames());
    }

    static class MapContentSchema extends MapSchema implements ContainerSizeSchemaTrait {
        public MapContentSchema(String subPrefix, BeanClass<?> type, Object expect, Data actual) {
            super(subPrefix, type, expect, actual);
        }

        @Override
        public Map<?, Object> getExpectValues(Object expect) {
            return (Map<?, Object>) expect;
        }
    }
}
