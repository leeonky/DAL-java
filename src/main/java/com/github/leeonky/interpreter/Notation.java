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
            O extends Operator<C, N, O>, T extends Parser<C, N, E, O, T>> NodeParser<C, N, E, O, T> nodeMatcher(
            Function<Token, N> factory) {
        return scanner -> scanner.getSourceCode().popWord(label)
                .map(t -> factory.apply(t).setPositionBegin(t.getPosition()));
    }

    public <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, T extends Parser<C, N, E, O, T>> OperatorParser<C, N, E, O, T> operatorMatcher(
            Supplier<O> factory, Predicate<T> predicate) {
        return scanner -> scanner.getSourceCode().popWord(label, () -> predicate.test(scanner))
                .map(token -> factory.get().setPosition(token.getPosition()));
    }

    public <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, T extends Parser<C, N, E, O, T>> OperatorParser<C, N, E, O, T> operatorMatcher(
            Supplier<O> factory) {
        return operatorMatcher(factory, scanner -> true);
    }
}
