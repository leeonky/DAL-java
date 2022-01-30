package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Stream;

import static java.util.Optional.empty;

public interface NodeParser<C extends RuntimeContext<C>, N extends Node<C, N>,
        E extends Expression<C, N, E, O>, O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> {

    static <E extends Expression<C, N, E, O>, N extends Node<C, N>, C extends RuntimeContext<C>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> NodeParser<C, N, E, O, P> oneOf(
            NodeParser<C, N, E, O, P> matcher, NodeParser<C, N, E, O, P>... matchers) {
        return procedure -> Stream.concat(Stream.of(matcher), Stream.of(matchers))
                .map(p -> p.parse(procedure)).filter(Optional::isPresent).findFirst().orElse(empty());
    }

    Optional<N> parse(P procedure);

    default NodeParser<C, N, E, O, P> map(Function<N, N> mapping) {
        return procedure -> parse(procedure).map(mapping);
    }

    default Mandatory<C, N, E, O, P> or(Mandatory<C, N, E, O, P> nodeFactory) {
        return procedure -> parse(procedure).orElseGet(() -> nodeFactory.parse(procedure));
    }

    default Mandatory<C, N, E, O, P> or(String message) {
        return procedure -> parse(procedure).orElseThrow(() -> procedure.getSourceCode().syntaxError(message, 0));
    }

    interface Mandatory<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> {

        N parse(P procedure);

        default Mandatory<C, N, E, O, P> map(Function<N, N> mapping) {
            return procedure -> mapping.apply(parse(procedure));
        }

        default Mandatory<C, N, E, O, P> withClause(ExpressionClauseParser.Mandatory<C, N, E, O, P> expressionClauseMandatory) {
            return procedure -> {
                N node = parse(procedure);
                return expressionClauseMandatory.parse(procedure).makeExpression(node);
            };
        }

        default Mandatory<C, N, E, O, P> recursive(ExpressionClauseParser<C, N, E, O, P> expressionClauseParser) {
            return procedure -> {
                N node = parse(procedure);
                Optional<ExpressionClause<C, N>> optionalNode = expressionClauseParser.parse(procedure);
                while (optionalNode.isPresent()) {
                    node = optionalNode.get().makeExpression(node);
                    optionalNode = expressionClauseParser.parse(procedure);
                }
                return node;
            };
        }
    }
}
