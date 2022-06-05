package com.github.leeonky.interpreter;

import java.util.Optional;

public interface ClauseParser<C extends RuntimeContext<C>, N extends Node<C, N>,
        E extends Expression<C, N, E, O>, O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>>
        extends Parser<C, N, E, O, P, ClauseParser<C, N, E, O, P>,
        ClauseParser.Mandatory<C, N, E, O, P>, Clause<C, N>> {

    @Override
    default ClauseParser<C, N, E, O, P> castParser(Parser<C, N, E, O, P, ClauseParser<C, N, E, O, P>,
            Mandatory<C, N, E, O, P>, Clause<C, N>> parser) {
        return parser::parse;
    }

    @Override
    default Mandatory<C, N, E, O, P> castMandatory(Parser.Mandatory<C, N, E, O, P, ClauseParser<C, N, E, O, P>,
            Mandatory<C, N, E, O, P>, Clause<C, N>> mandatory) {
        return mandatory::parse;
    }

    default ClauseParser<C, N, E, O, P> concat(ClauseParser<C, N, E, O, P> clause) {
        return procedure -> parse(procedure).map(c1 -> clause.parse(procedure).<Clause<C, N>>map(c2 -> previous ->
                c2.expression(c1.expression(previous))).orElse(c1));
    }

    default NodeParser<C, N, E, O, P> implicitNode(N input) {
        return procedure -> parse(procedure).map(clause -> clause.expression(input));
    }

    default Optional<N> parseAndMakeExpression(P procedure, N node) {
        return parse(procedure).map(clause -> clause.expression(node));
    }

    default N parseAndMakeExpressionOrInput(P procedure, N input) {
        return parseAndMakeExpression(procedure, input).orElse(input);
    }

    default N parseAndMakeExpressionOrInputRecursively(P procedure, N node) {
        N expression = parseAndMakeExpressionOrInput(procedure, node);
        if (expression == node)
            return expression;
        return parseAndMakeExpressionOrInputRecursively(procedure, expression);
    }

    interface Mandatory<C extends RuntimeContext<C>, N extends Node<C, N>,
            E extends Expression<C, N, E, O>, O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>>
            extends Parser.Mandatory<C, N, E, O, P, ClauseParser<C, N, E, O, P>, ClauseParser.Mandatory<C, N, E, O, P>,
            Clause<C, N>> {

        @Override
        default ClauseParser<C, N, E, O, P> castParser(Parser<C, N, E, O, P, ClauseParser<C, N, E, O, P>,
                Mandatory<C, N, E, O, P>, Clause<C, N>> parser) {
            return parser::parse;
        }

        default NodeParser.Mandatory<C, N, E, O, P> implicitNode(N node) {
            return procedure -> parse(procedure).expression(node);
        }
    }
}
