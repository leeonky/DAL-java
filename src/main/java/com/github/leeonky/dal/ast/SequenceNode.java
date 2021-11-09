package com.github.leeonky.dal.ast;

import java.util.Collections;
import java.util.Comparator;
import java.util.function.Function;

public class SequenceNode extends Node {
    public static final Comparator<Object> NOP_COMPARATOR = (o1, o2) -> 0;
    public static final SequenceNode NO_SEQUENCE = new SequenceNode(0, Type.AZ, null) {
        @Override
        public Comparator<Object> getComparator(Function<Object, Object> orderBy) {
            return NOP_COMPARATOR;
        }
    };
    private final int value;
    private final Type type;
    private final String word;

    public SequenceNode(int value, Type type, String word) {
        this.value = value;
        this.type = type;
        this.word = word;
    }

    public static SequenceNode noSequence() {
        return NO_SEQUENCE;
    }

    @Override
    public String inspect() {
        return value == 0 ? "" : String.join("", Collections.nCopies(value, word)) + " ";
    }

    @SuppressWarnings("unchecked")
    public Comparator<Object> getComparator(Function<Object, Object> orderBy) {
        return type.azOrZa(Comparator.comparing(o -> (Comparable<Object>) orderBy.apply(o)));
    }

    public enum Type {
        AZ, ZA {
            @Override
            Comparator<Object> azOrZa(Comparator<Object> comparator) {
                return comparator.reversed();
            }
        };

        Comparator<Object> azOrZa(Comparator<Object> comparator) {
            return comparator;
        }
    }

    public static Comparator<SequenceNode> comparator() {
        return Comparator.comparingInt(sequenceNode -> sequenceNode.value);
    }
}
