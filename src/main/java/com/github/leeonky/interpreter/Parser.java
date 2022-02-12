package com.github.leeonky.interpreter;

import java.util.List;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;

public interface Parser<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, OP extends Parser<C, N, E, O, P, OP, MA, T>,
        MA extends Parser.Mandatory<C, N, E, O, P, OP, MA, T>, T> {

    Optional<T> parse(P procedure);

    default MA or(MA mandatory) {
        return cast(procedure -> parse(procedure).orElseGet(() -> mandatory.parse(procedure)));
    }

    default MA mandatory(String message) {
        return cast(procedure -> parse(procedure).orElseThrow(() -> procedure.getSourceCode().syntaxError(message, 0)));
    }

    default MA repeatTo(Function<List<T>, T> factory) {
        return cast(procedure -> factory.apply(FunctionUtil.allOptional(() -> parse(procedure))));
    }

    default MA cast(Parser.Mandatory<C, N, E, O, P, OP, MA, T> mandatory) {
        throw new IllegalStateException();
    }

    interface Mandatory<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, OP extends Parser<C, N, E, O, P, OP, MA, T>,
            MA extends Parser.Mandatory<C, N, E, O, P, OP, MA, T>, T> {
        T parse(P procedure);

        default OP between(String opening, String closing, BiFunction<Token, T, T> factory) {
            return cast(procedure -> procedure.getSourceCode().popWord(opening).map(n -> {
                T parse = parse(procedure);
                procedure.getSourceCode().popWord(closing).orElseThrow(() ->
                        procedure.getSourceCode().syntaxError(String.format("should end with `%s`", closing), 0));
                return factory.apply(n, parse);
            }));
        }

        default OP cast(Parser<C, N, E, O, P, OP, MA, T> parser) {
            throw new IllegalStateException();
        }
    }
}
