package com.github.leeonky.dal.runtime.verifier.field;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.util.BeanClass;

import static com.github.leeonky.util.BeanClass.getClassName;

public class StructureExpectation extends Expectation {
    private final BeanClass<?> type;

    public StructureExpectation(String subPrefix, BeanClass<?> type) {
        super(subPrefix);
        this.type = type;
    }

    @Override
    protected String inspectExpect() {
        return String.format("type [%s]", type.getName());
    }

    @Override
    protected String inspectActual(Data actual) {
        return String.format("[%s]", getClassName(actual.getInstance()));
    }

    @Override
    protected boolean doVerify(Data actual) {
        return type.getType().isInstance(actual.getInstance());
    }

    public BeanClass<?> getType() {
        return type;
    }

    public static class SchemaType extends StructureExpectation {
        public SchemaType(String property, BeanClass<?> type) {
            super(property, type.getTypeArguments(0).orElseThrow(() -> Factory.illegalStateException(property)));
        }
    }
}
