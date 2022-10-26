package com.github.leeonky.dal.runtime.inspector;

@Deprecated
public interface TypeValueInspectorBk extends InspectorBk {
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
