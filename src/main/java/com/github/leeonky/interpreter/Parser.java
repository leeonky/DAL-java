package com.github.leeonky.interpreter;

import com.github.leeonky.interpreter.wip.Sequence;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;

public interface Parser<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, OP extends Parser<C, N, E, O, P, OP, MA, T>,
        MA extends Parser.Mandatory<C, N, E, O, P, OP, MA, T>, T> {

    static <P extends Procedure<?, ?, ?, ?, ?>> Sequence<P> endWith(Notation notation) {
        return new Sequence.DefaultSequence<P>().endWith(notation);
    }

    Optional<T> parse(P procedure);

    default MA castMandatory(Parser.Mandatory<C, N, E, O, P, OP, MA, T> mandatory) {
        throw new IllegalStateException();
    }

    default OP castParser(Parser<C, N, E, O, P, OP, MA, T> parser) {
        throw new IllegalStateException();
    }

    default MA or(MA mandatory) {
        return castMandatory(procedure -> parse(procedure).orElseGet(() -> mandatory.parse(procedure)));
    }

    default MA mandatory(String message) {
        return castMandatory(procedure -> parse(procedure).orElseThrow(() -> procedure.getSourceCode().syntaxError(message, 0)));
    }

    interface Mandatory<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, OP extends Parser<C, N, E, O, P, OP, MA, T>,
            MA extends Parser.Mandatory<C, N, E, O, P, OP, MA, T>, T> {
        T parse(P procedure);

        default OP castParser(Parser<C, N, E, O, P, OP, MA, T> parser) {
            throw new IllegalStateException();
        }

        default MA castMandatory(Parser.Mandatory<C, N, E, O, P, OP, MA, T> mandatory) {
            throw new IllegalStateException();
        }

        @Deprecated
        default OP between(String opening, String closing, BiFunction<Token, T, T> factory) {
            return castParser(procedure -> procedure.getSourceCode().popWord(opening).map(token -> {
                T result = parse(procedure);
                procedure.getSourceCode().popWord(closing).orElseThrow(() ->
                        procedure.getSourceCode().syntaxError(String.format("should end with `%s`", closing), 0));
                return factory.apply(token, result);
            }));
        }

        default MA closeBy(Sequence<? super P> sequence) {
            return castMandatory(procedure -> {
                T element = parse(procedure);
                sequence.close(procedure);
                return element;
            });
        }

        default MA sequence(Sequence<? super P> sequence, Function<List<T>, T> factory) {
            return castMandatory(procedure -> factory.apply(sequence.validate(procedure, new ArrayList<T>() {{
                while (!sequence.isClose(procedure)) {
                    add(parse(procedure));
                    if (!sequence.isSplitter(procedure))
                        break;
                }
                sequence.close(procedure);
            }})));
        }
    }
}
