package com.github.leeonky.interpreter;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;

import static com.github.leeonky.interpreter.IfThenFactory.when;

public interface Parser<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, OP extends Parser<C, N, E, O, P, OP, MA, T>,
        MA extends Parser.Mandatory<C, N, E, O, P, OP, MA, T>, T> {

    static <P extends Procedure<?, ?, ?, ?, ?>> Sequence<P> endWith(Notation notation) {
        return new Sequence.DefaultSequence<P>().endWith(notation);
    }

    Optional<T> parse(P procedure);

    default MA cast(Parser.Mandatory<C, N, E, O, P, OP, MA, T> mandatory) {
        throw new IllegalStateException();
    }

    default MA or(MA mandatory) {
        return cast(procedure -> parse(procedure).orElseGet(() -> mandatory.parse(procedure)));
    }

    default MA mandatory(String message) {
        return cast(procedure -> parse(procedure).orElseThrow(() -> procedure.getSourceCode().syntaxError(message, 0)));
    }

    default NodeParser<C, N, E, O, P> sequence(Sequence<? super P> sequence, Function<List<T>, N> factory) {
        return procedure -> {
            List<T> validate = sequence.validate(procedure, new ArrayList<T>() {{
                while (!sequence.isClose(procedure)) {
                    Optional<T> optional = parse(procedure);
                    if (optional.isPresent()) {
                        add(optional.get());
                        if (!sequence.isSplitter(procedure))
                            break;
                    } else
                        break;
                }
                sequence.close(procedure);
            }});
            return when(!validate.isEmpty()).optional(() -> factory.apply(validate));
        };
    }

    interface Mandatory<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, OP extends Parser<C, N, E, O, P, OP, MA, T>,
            MA extends Parser.Mandatory<C, N, E, O, P, OP, MA, T>, T> {
        T parse(P procedure);

        default MA cast(Parser.Mandatory<C, N, E, O, P, OP, MA, T> mandatory) {
            throw new IllegalStateException();
        }

        default MA closeBy(Sequence<? super P> sequence) {
            return cast(procedure -> {
                T element = parse(procedure);
                sequence.close(procedure);
                return element;
            });
        }

        default NodeParser.Mandatory<C, N, E, O, P> sequence(Sequence<? super P> sequence, Function<List<T>, N> factory) {
            return procedure -> factory.apply(sequence.validate(procedure, new ArrayList<T>() {{
                while (!sequence.isClose(procedure)) {
                    add(parse(procedure));
                    if (!sequence.isSplitter(procedure))
                        break;
                }
                sequence.close(procedure);
            }}));
        }
    }
}
