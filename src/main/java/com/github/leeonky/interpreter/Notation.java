package com.github.leeonky.interpreter;

import java.util.function.Function;

public class Notation {
    private final String label;

    public Notation(String label) {
        this.label = label;
    }

    public String getLabel() {
        return label;
    }

    public <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, T extends TokenParser<C, N, E, O, T>> NodeMatcher<C, N, E, O, T> asNode(Function<Token, N> factory) {
        return parser -> parser.getSourceCode().popWord(label)
                .map(t -> factory.apply(t).setPositionBegin(t.getPosition()));
    }
}
