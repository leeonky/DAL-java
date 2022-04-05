package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.function.Function;

public interface TokenScanner<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> {
    Optional<Token> scan(SourceCode sourceCode);

    default NodeParser<C, N, E, O, P> nodeParser(Function<Token, N> mapper) {
        return procedure -> scan(procedure.getSourceCode()).map(token ->
                mapper.apply(token).setPositionBegin(token.getPosition()));
    }

    interface Mandatory<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> {
        Token scan(SourceCode sourceCode);

        default NodeParser.Mandatory<C, N, E, O, P> nodeParser(Function<Token, N> mapper) {
            return procedure -> {
                Token token = scan(procedure.getSourceCode());
                return mapper.apply(token).setPositionBegin(token.getPosition());
            };
        }
    }
}
