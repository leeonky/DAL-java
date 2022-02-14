package com.github.leeonky.interpreter;

import com.github.leeonky.interpreter.wip.Closing;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;

public interface Parser<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, OP extends Parser<C, N, E, O, P, OP, MA, T>,
        MA extends Parser.Mandatory<C, N, E, O, P, OP, MA, T>, T> {

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
        return castMandatory(procedure -> parse(procedure).orElseThrow(() ->
                procedure.getSourceCode().syntaxError(message, 0)));
    }

    @Deprecated
    default MA repeat(Several<? super P> several, Function<List<T>, T> factory) {
        return castMandatory(procedure -> factory.apply(several.validate(procedure, new ArrayList<T>() {{
            while (!several.isClosing(procedure)) {
                Optional<T> result = parse(procedure);
                if (result.isPresent()) {
                    add(result.get());
                    if (!several.isSplitter(procedure))
                        break;
                } else
                    break;
            }
        }})));
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

        @Deprecated
        default MA repeat(Several<? super P> several, Function<List<T>, T> factory) {
            return castMandatory(procedure -> factory.apply(new ArrayList<T>() {{
                while (!several.isClosing(procedure)) {
                    add(parse(procedure));
                    if (!several.isSplitter(procedure))
                        break;
                }
            }}));
        }

        default MA closeBy(Closing<? super P> closing) {
            return castMandatory(procedure -> {
                T element = parse(procedure);
                closing.doClose(procedure);
                return element;
            });
        }
    }
}
