package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.stream.Stream;

import static java.util.Optional.empty;

public interface Parser<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, PA extends Parser<C, N, E, O, P, PA, MA, T>,
        MA extends Parser.Mandatory<C, N, E, O, P, PA, MA, T>, T> {

    @SuppressWarnings("unchecked")
    static <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, PA extends Parser<C, N, E, O, P, PA, MA, T>,
            MA extends Parser.Mandatory<C, N, E, O, P, PA, MA, T>, T> PA oneOf(PA... parsers) {
        return parsers[0].castParser(procedure -> Stream.of(parsers).map(operatorParser -> operatorParser.parse(procedure)).
                filter(Optional::isPresent).findFirst().orElse(empty()));
    }

    Optional<T> parse(P procedure);

    default PA castParser(Parser<C, N, E, O, P, PA, MA, T> parser) {
        throw new IllegalStateException();
    }

    default MA castMandatory(Parser.Mandatory<C, N, E, O, P, PA, MA, T> mandatory) {
        throw new IllegalStateException();
    }

    default MA or(MA mandatory) {
        return castMandatory(procedure -> parse(procedure).orElseGet(() -> mandatory.parse(procedure)));
    }

    default MA mandatory(String message) {
        return castMandatory(procedure -> parse(procedure).orElseThrow(() -> procedure.getSourceCode().syntaxError(message, 0)));
    }

    interface Mandatory<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, PA extends Parser<C, N, E, O, P, PA, MA, T>,
            MA extends Parser.Mandatory<C, N, E, O, P, PA, MA, T>, T> {

        default PA castParser(Parser<C, N, E, O, P, PA, MA, T> parser) {
            throw new IllegalStateException();
        }

        T parse(P procedure);
    }
}
