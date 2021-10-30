package com.github.leeonky.dal.ast;

import java.util.Collections;

public class SequenceNode extends Node {
    public static final SequenceNode NO_SEQUENCE = new SequenceNode(0, Type.AZ, null);
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

    public enum Type {
        AZ, ZA
    }
}
