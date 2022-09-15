package com.github.leeonky.dal.runtime.verifier.field;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.verifier.field.ContainerSchemaTrait.ContainerSchemaBK;
import com.github.leeonky.util.BeanClass;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static java.lang.String.format;

@Deprecated
class MapSchemaBK extends ContainerSchemaBK {
    public MapSchemaBK(String subPrefix, BeanClass<?> type, Object expect, Data actual) {
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

    static class MapContentSchemaBK extends MapSchemaBK implements ContainerSizeSchemaTrait {
        public MapContentSchemaBK(String subPrefix, BeanClass<?> type, Object expect, Data actual) {
            super(subPrefix, type, expect, actual);
        }

        @Override
        public Map<?, Object> getExpectValues(Object expect) {
            return (Map<?, Object>) expect;
        }
    }
}
