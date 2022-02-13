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

    default Mandatory<C, N, E, O, P> mandatory(String message) {
        return procedure -> parse(procedure).orElseThrow(() -> procedure.getSourceCode().syntaxError(message, 0));
    }

    default ClauseParser<C, N, E, O, P> clause(NodeParser.Mandatory<C, N, E, O, P> nodeFactory) {
        return procedure -> parse(procedure).map(operator -> procedure.underOperator(operator, () -> {
            N right = nodeFactory.parse(procedure);
            return left -> procedure.createExpression(left, operator, right);
        }));
    }

    default ClauseParser<C, N, E, O, P> clause(NodeParser<C, N, E, O, P> nodeParser) {
        return procedure -> procedure.getSourceCode().tryFetch(() -> parse(procedure).map(operator ->
                procedure.underOperator(operator, () -> nodeParser.parse(procedure).<Clause<C, N>>map(n ->
                        left -> procedure.createExpression(left, operator, n)).orElse(null))));
    }

    default NodeParser<C, N, E, O, P> unary(NodeParser.Mandatory<C, N, E, O, P> nodeFactory) {
        return procedure -> parse(procedure).map(operator -> procedure.underOperator(operator, () ->
                procedure.createExpression(null, operator, nodeFactory.parse(procedure))));
    }

    interface Mandatory<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> {
        O parse(P procedure);

        default ClauseParser.Mandatory<C, N, E, O, P> clause(NodeParser.Mandatory<C, N, E, O, P> nodeFactory) {
            return procedure -> {
                O operator = parse(procedure);
                return procedure.underOperator(operator, () -> {
                    N right = nodeFactory.parse(procedure);
                    return left -> procedure.createExpression(left, operator, right);
                });
            };
        }
    }
}
