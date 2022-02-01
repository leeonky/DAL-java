package com.github.leeonky.interpreter;

import java.util.List;
import java.util.function.Function;

import static com.github.leeonky.interpreter.SourceCode.FetchBy.BY_NODE;

public class ComplexNode<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, M extends Parser.Mandatory<C, N, E, O, P, T>,
        T, A> {
    private final Type type;
    private final M mandatory;
    private String message;

    public ComplexNode(Type type, M mandatory, String message) {
        this.type = type;
        this.mandatory = mandatory;
        this.message = message;
    }

    public static <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>,
            M extends Parser.Mandatory<C, N, E, O, P, T>, T> ComplexNode<C, N, E, O, P, M, T, T> single(M mandatory, String message) {
        return new ComplexNode<>(Type.SINGLE, mandatory, message);
    }

    public static <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>,
            M extends Parser.Mandatory<C, N, E, O, P, T>, T> ComplexNode<C, N, E, O, P, M, T, List<T>> multiple(M mandatory) {
        return new ComplexNode<>(Type.MULTIPLE, mandatory, null);
    }

    public OpeningClosing between(char opening, char closing) {
        return new OpeningClosing(opening, closing);
    }

    public enum Type {
        SINGLE, MULTIPLE
    }

    public class OpeningClosing {
        private final char opening, closing;

        public OpeningClosing(char opening, char closing) {
            this.opening = opening;
            this.closing = closing;
        }

        @SuppressWarnings("unchecked")
        public NodeParser<C, N, E, O, P> nodeParser(Function<A, N> nodeFactory) {
            return procedure -> procedure.getSourceCode().fetchElementNode(BY_NODE, opening,
                    () -> mandatory.parse(procedure), closing, args -> {
                        if (type == Type.MULTIPLE)
                            return nodeFactory.apply((A) args);
                        if (args.size() != 1)
                            throw procedure.getSourceCode().syntaxError(message, -1);
                        return nodeFactory.apply((A) args.get(0));
                    });
        }

    }
}
