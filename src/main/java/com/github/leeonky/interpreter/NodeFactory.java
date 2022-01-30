package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.function.Function;

public interface NodeFactory<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>, S extends Scanner<C, N, E, O, S>> {

    N fetch(S scanner);

    default NodeFactory<C, N, E, O, S> map(Function<N, N> mapping) {
        return scanner -> mapping.apply(fetch(scanner));
    }

    default NodeFactory<C, N, E, O, S> withClause(ExpressionClauseFactory<C, N, E, O, S> expressionClauseFactory) {
        return scanner -> {
            N node = fetch(scanner);
            return expressionClauseFactory.fetch(scanner).makeExpression(node);
        };
    }

    default NodeFactory<C, N, E, O, S> recursive(ExpressionClauseMatcher<C, N, E, O, S> expressionClauseMatcher) {
        return scanner -> {
            N node = fetch(scanner);
            Optional<ExpressionClause<C, N>> optionalNode = expressionClauseMatcher.fetch(scanner);
            while (optionalNode.isPresent()) {
                node = optionalNode.get().makeExpression(node);
                optionalNode = expressionClauseMatcher.fetch(scanner);
            }
            return node;
        };
    }
}
