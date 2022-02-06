package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;

public interface TokenScanner<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> {
    Optional<Token> scan(SourceCode sourceCode);

    default Mandatory mandatory(String message) {
        return sourceCode -> scan(sourceCode).orElseThrow(() -> sourceCode.syntaxError(message, 0));
    }

    default NodeParser<C, N, E, O, P> nodeParser(Function<Token, N> mapper) {
        return procedure -> scan(procedure.getSourceCode()).map(token ->
                mapper.apply(token).setPositionBegin(token.getPosition()));
    }

    default ClauseParser<C, N, E, O, P> clauseParser(BiFunction<Token, N, N> mapper) {
        return procedure -> scan(procedure.getSourceCode()).map(token -> previous ->
                mapper.apply(token, previous).setPositionBegin(token.getPosition()));
    }

    @Deprecated
    interface Mandatory {
        Token scan(SourceCode sourceCode);

        default <E extends Expression<C, N, E, O>, N extends Node<C, N>, C extends RuntimeContext<C>,
                O extends Operator<C, N, O>, S extends Procedure<C, N, E, O, S>> NodeParser.Mandatory<C, N, E, O, S> map(
                Function<Token, N> mapper) {
            return procedure -> {
                Token token = scan(procedure.getSourceCode());
                return mapper.apply(token).setPositionBegin(token.getPosition());
            };
        }
    }
}
