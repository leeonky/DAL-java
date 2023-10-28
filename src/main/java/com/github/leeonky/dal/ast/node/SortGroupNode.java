package com.github.leeonky.dal.ast.node;

import com.github.leeonky.dal.runtime.Data;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.joining;

public class SortGroupNode extends DALNode {
    public static final Comparator<Data> NOP_COMPARATOR = (o1, o2) -> 0;
    public static final SortGroupNode NO_SEQUENCE = new SortGroupNode(Collections.emptyList()) {
        @Override
        public Comparator<Data> comparator(Function<Data, Object> orderBy) {
            return NOP_COMPARATOR;
        }
    };
    private final List<SortSymbolNode> sortSymbolNodes;

    public SortGroupNode(List<DALNode> nodes) {
        sortSymbolNodes = nodes.stream().map(SortSymbolNode.class::cast).collect(Collectors.toList());
    }

    public static SortGroupNode noSequence() {
        return NO_SEQUENCE;
    }

    @Override
    public String inspect() {
        return !sortSymbolNodes.isEmpty() ? sortSymbolNodes.stream().map(SortSymbolNode::inspect)
                .collect(joining("", "", " ")) : "";
    }

    @SuppressWarnings("unchecked")
    public Comparator<Data> comparator(Function<Data, Object> orderBy) {
        return sortSymbolNodes.get(0).getType().azOrZa(Comparator.comparing(o -> (Comparable<Object>) orderBy.apply(o)));
    }

    public static Comparator<SortGroupNode> comparator() {
        return Comparator.comparingInt(sequenceNode -> sequenceNode.sortSymbolNodes.size());
    }
}
