package com.github.leeonky.interpreter;

import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

public class Notation {
    private final String label;

    private Notation(String label) {
        this.label = label;
    }

    public static Notation notation(String label) {
        return new Notation(label);
    }

    public String getLabel() {
        return label;
    }

    public <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, T extends Scanner<C, N, E, O, T>> NodeMatcher<C, N, E, O, T> nodeMatcher(
            Function<Token, N> factory) {
        return parser -> parser.getSourceCode().popWord(label)
                .map(t -> factory.apply(t).setPositionBegin(t.getPosition()));
    }

    public <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, T extends Scanner<C, N, E, O, T>> OperatorMatcher<C, N, E, O, T> operatorMatcher(
            Supplier<O> factory, Predicate<T> predicate) {
        return tokenParser -> tokenParser.getSourceCode().popWord(label, () -> predicate.test(tokenParser))
                .map(token -> factory.get().setPosition(token.getPosition()));
    }

    public <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, T extends Scanner<C, N, E, O, T>> OperatorMatcher<C, N, E, O, T> operatorMatcher(
            Supplier<O> factory) {
        return operatorMatcher(factory, parser -> true);
    }
}
