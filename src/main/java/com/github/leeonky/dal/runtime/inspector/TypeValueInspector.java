package com.github.leeonky.dal.runtime.inspector;

public interface TypeValueInspector extends Inspector {
    String inspectType();

    String inspectValue();

    @Override
    default String inspect(String path, InspectorCache cache) {
        return String.join("\n", inspectType(), inspectValue()).trim();
    }

    @Override
    default String dump(String path, InspectorCache cache) {
        return String.join(" ", inspectType(), inspectValue()).trim();
    }
}
