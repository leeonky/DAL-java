package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;

public interface TokenScanner<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>, S extends Parser<C, N, E, O, S>> {
    Optional<Token> scan(SourceCode sourceCode);

    default Mandatory or(String message) {
        return sourceCode -> scan(sourceCode).orElseThrow(() -> sourceCode.syntaxError(message, 0));
    }

    default NodeParser<C, N, E, O, S> nodeMatcher(Function<Token, N> mapper) {
        return scanner -> scan(scanner.getSourceCode()).map(token ->
                mapper.apply(token).setPositionBegin(token.getPosition()));
    }

    default ExpressionClauseParser<C, N, E, O, S> toClauseMatcher(BiFunction<Token, N, N> mapper) {
        return scanner -> scan(scanner.getSourceCode()).map(token -> previous ->
                mapper.apply(token, previous).setPositionBegin(token.getPosition()));
    }

    interface Mandatory {
        Token scan(SourceCode sourceCode);

        default <E extends Expression<C, N, E, O>, N extends Node<C, N>, C extends RuntimeContext<C>,
                O extends Operator<C, N, O>, S extends Parser<C, N, E, O, S>> NodeParser.Mandatory<C, N, E, O, S> map(
                Function<Token, N> mapper) {
            return scanner -> {
                Token token = scan(scanner.getSourceCode());
                return mapper.apply(token).setPositionBegin(token.getPosition());
            };
        }
    }
}
