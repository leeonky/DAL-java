package com.github.leeonky.dal.ast;

import java.util.Collections;
import java.util.Comparator;
import java.util.function.Function;

public class SequenceNode extends Node {
    public static final SequenceNode NO_SEQUENCE = new SequenceNode(0, Type.AZ, null) {
        @Override
        public Comparator<Object> getComparator(Function<Object, Object> orderBy) {
            return (o1, o2) -> 0;
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

    public int getValue() {
        return value;
    }

    public Type getType() {
        return type;
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
}
