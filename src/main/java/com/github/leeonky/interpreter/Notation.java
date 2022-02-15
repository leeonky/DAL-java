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
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> NodeParser<C, N, E, O, P> nodeParser(
            Function<String, N> factory) {
        return procedure -> procedure.getSourceCode().popWord(this).map(token ->
                factory.apply(token.getContent()).setPositionBegin(token.getPosition()));
    }

    public <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> OperatorParser<C, N, E, O, P> operatorParser(
            Supplier<O> factory, Predicate<P> predicate) {
        return procedure -> procedure.getSourceCode().popWord(this, () -> predicate.test(procedure))
                .map(token -> factory.get().setPosition(token.getPosition()));
    }

    public <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> OperatorParser<C, N, E, O, P> operatorParser(
            Supplier<O> factory) {
        return operatorParser(factory, procedure -> true);
    }

    public <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> NodeParser<C, N, E, O, P> next(
            NodeParser.Mandatory<C, N, E, O, P> mandatory) {
        return procedure -> procedure.getSourceCode().popWord(this)
                .map(t -> mandatory.parse(procedure).setPositionBegin(t.getPosition()));
    }

    public int length() {
        return label.length();
    }
}
