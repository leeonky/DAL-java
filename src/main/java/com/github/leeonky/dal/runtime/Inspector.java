package com.github.leeonky.dal.runtime;

public interface Inspector {
    String inspectType();

    String inspectValue();

    default String inspect() {
        return String.join("\n", inspectType(), inspectValue()).trim();
    }

    default String dump() {
        return String.join(" ", inspectType(), inspectValue()).trim();
    }
}
