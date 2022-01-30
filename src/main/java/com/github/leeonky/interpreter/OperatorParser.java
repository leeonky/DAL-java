package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.stream.Stream;

import static java.util.Optional.empty;

public interface OperatorParser<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> {
    static <E extends Expression<C, N, E, O>, N extends Node<C, N>, C extends RuntimeContext<C>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> OperatorParser<C, N, E, O, P> oneOf(
            OperatorParser<C, N, E, O, P>... parsers) {
        return procedure -> Stream.of(parsers).map(operatorParser -> operatorParser.parse(procedure)).
                filter(Optional::isPresent).findFirst().orElse(empty());
    }

    Optional<O> parse(P procedure);

    default Mandatory<C, N, E, O, P> or(Mandatory<C, N, E, O, P> compiler) {
        return procedure -> parse(procedure).orElseGet(() -> compiler.parse(procedure));
    }

    default Mandatory<C, N, E, O, P> or(String message) {
        return procedure -> parse(procedure).orElseThrow(() -> procedure.getSourceCode().syntaxError(message, 0));
    }

    default ClauseParser<C, N, E, O, P> clause(NodeParser.Mandatory<C, N, E, O, P> nodeFactory) {
        return procedure -> parse(procedure).map(operator -> procedure.fetchClause(operator, nodeFactory));
    }

    interface Mandatory<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> {
        O parse(P procedure);

        default ClauseParser.Mandatory<C, N, E, O, P> mandatoryClause(NodeParser.Mandatory<C, N, E, O, P> nodeFactory) {
            return procedure -> procedure.fetchClause(parse(procedure), nodeFactory);
        }
    }
}
