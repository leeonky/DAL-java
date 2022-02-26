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
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> NodeParser<C, N, E, O, P> before(
            NodeParser<C, N, E, O, P> nodeParser) {
        return procedure -> procedure.getSourceCode().tryFetch(() -> getToken(procedure)
                .flatMap(t -> nodeParser.parse(procedure)));
    }

    public <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> NodeParser<C, N, E, O, P> before(
            NodeParser.Mandatory<C, N, E, O, P> mandatory) {
        return procedure -> getToken(procedure).map(t -> mandatory.parse(procedure));
    }

    //    TODO refactor merge all before
    public <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> ClauseParser<C, N, E, O, P> before(
            ClauseParser.Mandatory<C, N, E, O, P> mandatory) {
        return procedure -> getToken(procedure).map(t -> mandatory.parse(procedure));
    }

    public int length() {
        return label.length();
    }

//    public Notation with(Notation... others) {
//        List<Notation> notations = new ArrayList<Notation>() {{
//            add(Notation.this);
//            addAll(asList(others));
//        }};
//
//        return new Notation(notations.stream().map(Notation::getLabel).collect(Collectors.joining(" "))) {
//            @Override
//            protected <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
//                    O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> Optional<Token> getToken(
//                    P procedure, Predicate<P> predicate) {
//                procedure.getSourceCode().tryFetch(() -> procedure.positionOf(position -> when(notations.stream()
//                        .map(notation -> notation.getToken(procedure, predicate)).filter(Optional::isPresent)
//                        .count() == notations.size()).optional(() -> new Token(position).append(getLabel()))));
//                return super.getToken(procedure, predicate);
//            }
//        };
//    }
}
