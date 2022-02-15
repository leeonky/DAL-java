package com.github.leeonky.dal.ast;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.joining;

public class SortSequenceNode extends DALNode {
    public static final Comparator<Object> NOP_COMPARATOR = (o1, o2) -> 0;
    public static final SortSequenceNode NO_SEQUENCE = new SortSequenceNode(Collections.emptyList()) {
        @Override
        public Comparator<Object> getComparator(Function<Object, Object> orderBy) {
            return NOP_COMPARATOR;
        }
    };
    private final List<SortNode> sortNodes;

    public SortSequenceNode(List<DALNode> nodes) {
        sortNodes = nodes.stream().map(SortNode.class::cast).collect(Collectors.toList());
    }

    public static SortSequenceNode noSequence() {
        return NO_SEQUENCE;
    }

    @Override
    public String inspect() {
        return !sortNodes.isEmpty() ? sortNodes.stream().map(SortNode::inspect).collect(joining("", "", " ")) : "";
    }

    @SuppressWarnings("unchecked")
    public Comparator<Object> getComparator(Function<Object, Object> orderBy) {
        return sortNodes.get(0).getType().azOrZa(Comparator.comparing(o -> (Comparable<Object>) orderBy.apply(o)));
    }

    public static Comparator<SortSequenceNode> comparator() {
        return Comparator.comparingInt(sequenceNode -> sequenceNode.sortNodes.size());
    }
}
