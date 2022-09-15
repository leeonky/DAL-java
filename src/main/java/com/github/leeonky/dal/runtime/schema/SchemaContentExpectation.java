package com.github.leeonky.dal.runtime.schema;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.util.BeanClass;

public class SchemaContentExpectation extends SchemaExpectation {
    public SchemaContentExpectation(String property, BeanClass<?> type, Data actual, Object expect) {
        super(property, type, actual, expect);
    }

}
