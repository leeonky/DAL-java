package com.github.leeonky.interpreter;

import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

public interface NodeParser<C extends RuntimeContext<C>, N extends Node<C, N>,
        E extends Expression<C, N, E, O>, O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>>
        extends Parser<C, N, E, O, P, NodeParser<C, N, E, O, P>, NodeParser.Mandatory<C, N, E, O, P>, N> {

    static <E extends Expression<C, N, E, O>, N extends Node<C, N>, C extends RuntimeContext<C>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> NodeParser<C, N, E, O, P> lazy(
            Supplier<NodeParser<C, N, E, O, P>> parser) {
        return procedure -> parser.get().parse(procedure);
    }

    @Override
    default NodeParser<C, N, E, O, P> castParser(Parser<C, N, E, O, P, NodeParser<C, N, E, O, P>,
            Mandatory<C, N, E, O, P>, N> parser) {
        return parser::parse;
    }

    @Override
    default Mandatory<C, N, E, O, P> castMandatory(Parser.Mandatory<C, N, E, O, P, NodeParser<C, N, E, O, P>,
            NodeParser.Mandatory<C, N, E, O, P>, N> mandatory) {
        return mandatory::parse;
    }

    default ClauseParser<C, N, E, O, P> ignoreInput() {
        return procedure -> parse(procedure).map(node -> p -> node);
    }

    default ClauseParser<C, N, E, O, P> clauseParser(BiFunction<N, N, N> biFunction) {
        return procedure -> parse(procedure).map(n -> input -> biFunction.apply(input, n));
    }

    default NodeParser<C, N, E, O, P> expression(ClauseParser.Mandatory<C, N, E, O, P> mandatory) {
        return procedure -> parse(procedure).map(node -> mandatory.parse(procedure).expression(node));
    }

    default NodeParser<C, N, E, O, P> concat(ClauseParser<C, N, E, O, P> clauseParser) {
        return procedure -> parse(procedure).map(node -> clauseParser.concated(procedure, node));
    }

    default NodeParser<C, N, E, O, P> recursive(ClauseParser<C, N, E, O, P> clauseParser) {
        return procedure -> parse(procedure).map(node -> clauseParser.recursived(procedure, node));
    }

    interface Mandatory<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> extends
            Parser.Mandatory<C, N, E, O, P, NodeParser<C, N, E, O, P>, NodeParser.Mandatory<C, N, E, O, P>, N> {

        @Override
        default NodeParser<C, N, E, O, P> castParser(Parser<C, N, E, O, P, NodeParser<C, N, E, O, P>,
                Mandatory<C, N, E, O, P>, N> parser) {
            return parser::parse;
        }

        default Mandatory<C, N, E, O, P> map(Function<N, N> mapping) {
            return procedure -> mapping.apply(parse(procedure));
        }

        default Mandatory<C, N, E, O, P> expression(ClauseParser.Mandatory<C, N, E, O, P> clauseMandatory) {
            return procedure -> {
                N node = parse(procedure);
                return clauseMandatory.parse(procedure).expression(node);
            };
        }

        default NodeParser<C, N, E, O, P> combine(ClauseParser<C, N, E, O, P> clauseParser) {
            return procedure -> procedure.getSourceCode().tryFetch(() ->
                    clauseParser.combined(procedure, parse(procedure)));
        }

        default Mandatory<C, N, E, O, P> concat(ClauseParser<C, N, E, O, P> clauseParser) {
            return procedure -> clauseParser.concated(procedure, parse(procedure));
        }

        default Mandatory<C, N, E, O, P> recursive(ClauseParser<C, N, E, O, P> clauseParser) {
            return procedure -> clauseParser.recursived(procedure, parse(procedure));
        }

        static <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>, O extends
                Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> ClauseParser.Mandatory<C, N, E, O, P> clause(
                Function<N, Mandatory<C, N, E, O, P>> mandatoryFactory) {
            return procedure -> input -> mandatoryFactory.apply(input).parse(procedure);
        }
    }
}
