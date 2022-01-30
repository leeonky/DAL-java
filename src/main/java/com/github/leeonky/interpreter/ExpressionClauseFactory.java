package com.github.leeonky.interpreter;

public interface ExpressionClauseFactory<C extends RuntimeContext<C>, N extends Node<C, N>,
        E extends Expression<C, N, E, O>, O extends Operator<C, N, O>, S extends Scanner<C, N, E, O, S>> {
    ExpressionClause<C, N> fetch(S scanner);

    default NodeFactory<C, N, E, O, S> input(N node) {
        return scanner -> fetch(scanner).makeExpression(node);
    }

    //    TODO token => to class like operaterMatcher::toClause
    @Deprecated
    static <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>, O extends Operator<C, N, O>,
            S extends Scanner<C, N, E, O, S>> ExpressionClauseMatcher<C, N, E, O, S> after(
            String token, ExpressionClauseFactory<C, N, E, O, S> expressionClauseFactory) {
        return scanner -> scanner.fetchClauseAfter(token, expressionClauseFactory);
    }
}
