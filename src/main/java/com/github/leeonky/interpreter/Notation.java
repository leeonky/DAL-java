package com.github.leeonky.interpreter;

import java.util.Optional;
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
        return procedure -> getToken(procedure).map(token ->
                factory.apply(token.getContent()).setPositionBegin(token.getPosition()));
    }

    private <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> Optional<Token> getToken(P procedure) {
        return getToken(procedure, p -> true);
    }

    public <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> OperatorParser<C, N, E, O, P> operatorParser(
            Supplier<O> factory, Predicate<P> predicate) {
        return procedure -> getToken(procedure, predicate)
                .map(token -> factory.get().setPosition(token.getPosition()));
    }

    protected <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> Optional<Token> getToken(
            P procedure, Predicate<P> predicate) {
        return procedure.getSourceCode().popWord(this, () -> predicate.test(procedure));
    }

    public <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> OperatorParser<C, N, E, O, P> operatorParser(
            Supplier<O> factory) {
        return operatorParser(factory, procedure -> true);
    }

    public <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> NodeParser<C, N, E, O, P> and(
            NodeParser.Mandatory<C, N, E, O, P> mandatory) {
        return procedure -> getToken(procedure).map(t -> mandatory.parse(procedure).setPositionBegin(t.getPosition()));
    }

    public <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, OP extends Parser<C, N, E, O, P, OP, MA, T>,
            MA extends Parser.Mandatory<C, N, E, O, P, OP, MA, T>, T> OP before(OP op) {
        return op.castParser(procedure -> procedure.getSourceCode().tryFetch(() -> getToken(procedure)
                .flatMap(t -> op.parse(procedure))));
    }

    public <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, OP extends Parser<C, N, E, O, P, OP, MA, T>,
            MA extends Parser.Mandatory<C, N, E, O, P, OP, MA, T>, T> OP before(MA ma) {
        return ma.castParser(procedure -> getToken(procedure).map(t -> ma.parse(procedure)));
    }

    public int length() {
        return label.length();
    }
}
