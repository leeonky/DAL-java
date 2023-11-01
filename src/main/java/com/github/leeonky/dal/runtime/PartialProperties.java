package com.github.leeonky.dal.runtime;

import java.util.LinkedHashSet;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

class PartialProperties {
    final Object prefix;
    final Data partialData;
    final Set<Object> postfixes = new LinkedHashSet<>();
    final PartialPropertyStack partialPropertyStack = new PartialPropertyStack();

    public PartialProperties(Object prefix, Data partialData) {
        this.prefix = prefix;
        this.partialData = partialData;
    }

    public Set<String> collectPartialProperties(Data data) {
        postfixes.addAll(partialPropertyStack.collectPartialProperties(partialData));
        return postfixes.stream().map(property -> ((PartialObject) partialData.instance())
                        .removeExpectedField(data.fieldNames(), prefix, property))
                .filter(Optional::isPresent).map(Optional::get).collect(Collectors.toSet());
    }

    public boolean appendPartialProperties(Object symbol) {
        return postfixes.add(symbol);
    }
}
