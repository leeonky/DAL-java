package com.github.leeonky.interpreter;

import java.util.function.BiFunction;

import static java.util.Optional.ofNullable;

public interface NodeParser<C extends RuntimeContext<C>, N extends Node<C, N>,
        E extends Expression<C, N, E, O>, O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>>
        extends Parser<C, N, E, O, P, NodeParser<C, N, E, O, P>, NodeParser.Mandatory<C, N, E, O, P>, N> {

    static <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>, O extends
            Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> NodeParser.Mandatory<C, N, E, O, P> positionNode(
            NodeParser.Mandatory<C, N, E, O, P> mandatory) {
        return procedure -> procedure.positionOf(position -> mandatory.parse(procedure).setPositionBegin(position));
    }

    static <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>, O extends
            Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> NodeParser<C, N, E, O, P> positionNode(
            NodeParser<C, N, E, O, P> mandatory) {
        return procedure -> procedure.positionOf(position -> mandatory.parse(procedure)
                .map(node -> node.setPositionBegin(position)));
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

    default NodeParser<C, N, E, O, P> concat(ClauseParser.Mandatory<C, N, E, O, P> mandatory) {
        return procedure -> parse(procedure).map(node -> mandatory.parse(procedure).expression(node));
    }

    default NodeParser<C, N, E, O, P> concat(ClauseParser<C, N, E, O, P> clauseParser) {
        return procedure -> parse(procedure).map(node -> clauseParser.parseAndMakeExpressionOrInput(procedure, node));
    }

    default NodeParser<C, N, E, O, P> concatAll(ClauseParser<C, N, E, O, P> clauseParser) {
        return procedure -> parse(procedure).map(node -> clauseParser.parseAndMakeExpressionOrInputContinuously(procedure, node));
    }

    default NodeParser<C, N, E, O, P> with(ClauseParser<C, N, E, O, P> clauseParser) {
        return procedure -> procedure.getSourceCode().tryFetch(() -> parse(procedure)
                .flatMap(node -> clauseParser.parseAndMakeExpression(procedure, node)));
    }

    default NodeParser<C, N, E, O, P> with(ClauseParser.Mandatory<C, N, E, O, P> mandatory) {
        return procedure -> procedure.getSourceCode().tryFetch(() -> parse(procedure)
                .flatMap(node -> ofNullable(mandatory.parse(procedure).expression(node))));
    }

    default ClauseParser<C, N, E, O, P> clause(BiFunction<N, N, N> biFunction) {
        return procedure -> parse(procedure).map(n -> input -> biFunction.apply(input, n));
    }

    interface Mandatory<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> extends
            Parser.Mandatory<C, N, E, O, P, NodeParser<C, N, E, O, P>, NodeParser.Mandatory<C, N, E, O, P>, N> {

        @Override
        default NodeParser<C, N, E, O, P> castParser(Parser<C, N, E, O, P, NodeParser<C, N, E, O, P>,
                Mandatory<C, N, E, O, P>, N> parser) {
            return parser::parse;
        }

        default Mandatory<C, N, E, O, P> concat(ClauseParser.Mandatory<C, N, E, O, P> clauseMandatory) {
            return procedure -> {
                N node = parse(procedure);
                return clauseMandatory.parse(procedure).expression(node);
            };
        }

        default Mandatory<C, N, E, O, P> concat(ClauseParser<C, N, E, O, P> clauseParser) {
            return procedure -> clauseParser.parseAndMakeExpressionOrInput(procedure, parse(procedure));
        }

        default Mandatory<C, N, E, O, P> concatAll(ClauseParser<C, N, E, O, P> clauseParser) {
            return procedure -> clauseParser.parseAndMakeExpressionOrInputContinuously(procedure, parse(procedure));
        }

        default NodeParser<C, N, E, O, P> with(ClauseParser<C, N, E, O, P> clauseParser) {
            return procedure -> procedure.getSourceCode().tryFetch(() ->
                    clauseParser.parseAndMakeExpression(procedure, parse(procedure)));
        }

        default NodeParser.Mandatory<C, N, E, O, P> with(ClauseParser.Mandatory<C, N, E, O, P> mandatory) {
            return procedure -> {
                N input = parse(procedure);
                return mandatory.parse(procedure).expression(input);
            };
        }
    }
}
