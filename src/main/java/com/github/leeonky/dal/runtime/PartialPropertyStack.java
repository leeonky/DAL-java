package com.github.leeonky.dal.runtime;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

class PartialPropertyStack {
    private final Map<Data, PartialProperties> partials = new HashMap<>();

    public void setupPartialProperties(Object prefix, Data partial) {
        partials.put(partial, new PartialProperties(prefix, partial));
    }

    public PartialProperties fetchPartialProperties(Data instance) {
        PartialProperties partialProperties = partials.get(instance);
        if (partialProperties == null)
            partialProperties = partials.values().stream()
                    .map(sub -> sub.partialPropertyStack.fetchPartialProperties(instance))
                    .filter(Objects::nonNull).findFirst().orElse(null);
        return partialProperties;
    }

    public Set<String> collectPartialProperties(Data data) {
        return partials.values().stream().flatMap(partialProperties ->
                partialProperties.collectPartialProperties(data).stream()).collect(Collectors.toSet());
    }
}
