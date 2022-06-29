package com.github.leeonky.dal.runtime;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

public interface PartialObject {
    default Optional<String> removeExpectedField(Set<Object> fields, Object prefix, Object postfix) {
        if (postfix instanceof String) {
            List<String> removed = fields.stream().filter(String.class::isInstance).map(Object::toString)
                    .filter(field -> predicate(field, buildField(prefix, postfix)))
                    .collect(Collectors.toList());
            if (removed.size() > 1)
                //        TODO need test
                throw new IllegalArgumentException("More than one expected field found: " + removed);
            return removed.stream().findFirst();
        }
        return Optional.empty();
    }

    default boolean predicate(String candidate, String field) {
        return candidate.equalsIgnoreCase(field);
    }

    default String buildField(Object prefix, Object postfix) {
        return String.format("%s%s", prefix, postfix);
    }
}