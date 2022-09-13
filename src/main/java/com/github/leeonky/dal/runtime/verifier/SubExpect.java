package com.github.leeonky.dal.runtime.verifier;

import com.github.leeonky.util.BeanClass;

import java.util.List;
import java.util.Map;

public class SubExpect {
    private final Map<?, Object> expectValues;
    private final List<?> actualProperties;
    private final String fieldFormat;

    public SubExpect(Map<?, Object> expectValues, List<?> actualProperties, String format) {
        this.expectValues = expectValues;
        this.actualProperties = actualProperties;
        fieldFormat = format;
    }

    public Map<?, Object> getExpectValues() {
        return expectValues;
    }

    public List<?> getActualProperties() {
        return actualProperties;
    }

    public String getFieldFormat() {
        return fieldFormat;
    }

    public BeanClass<?> subType(BeanClass<?> type) {
        return type.getElementType();
    }

    public String subPrefix(String subPrefix, Object property) {
        return String.format(getFieldFormat(), subPrefix, property);
    }

}
