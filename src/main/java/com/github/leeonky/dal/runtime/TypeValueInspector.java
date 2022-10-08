package com.github.leeonky.dal.runtime;

public interface TypeValueInspector extends Inspector {
    String inspectType();

    String inspectValue();

    @Override
    default String inspect() {
        return String.join("\n", inspectType(), inspectValue()).trim();
    }

    @Override
    default String dump() {
        return String.join(" ", inspectType(), inspectValue()).trim();
    }
}
