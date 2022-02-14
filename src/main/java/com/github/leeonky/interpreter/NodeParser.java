package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Stream;

import static com.github.leeonky.interpreter.FunctionUtil.allOptional;
import static java.util.Optional.empty;

public interface NodeParser<C extends RuntimeContext<C>, N extends Node<C, N>,
        E extends Expression<C, N, E, O>, O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>>
        extends ParserPx<C, N, E, O, P, N>, Parser<C, N, E, O, P, NodeParser<C, N, E, O, P>,
        NodeParser.Mandatory<C, N, E, O, P>, N> {

    static <E extends Expression<C, N, E, O>, N extends Node<C, N>, C extends RuntimeContext<C>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> NodeParser<C, N, E, O, P> oneOf(
            NodeParser<C, N, E, O, P>... matchers) {
        return procedure -> Stream.of(matchers)
                .map(p -> p.parse(procedure)).filter(Optional::isPresent).findFirst().orElse(empty());
    }

    static <E extends Expression<C, N, E, O>, N extends Node<C, N>, C extends RuntimeContext<C>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> NodeParser<C, N, E, O, P> lazy(
            Supplier<NodeParser<C, N, E, O, P>> parser) {
        return procedure -> parser.get().parse(procedure);
    }

    @Override
    default Mandatory<C, N, E, O, P> castMandatory(Parser.Mandatory<C, N, E, O, P, NodeParser<C, N, E, O, P>,
            NodeParser.Mandatory<C, N, E, O, P>, N> mandatory) {
        return mandatory::parse;
    }

    @Override
    default NodeParser<C, N, E, O, P> castParser(Parser<C, N, E, O, P, NodeParser<C, N, E, O, P>,
            Mandatory<C, N, E, O, P>, N> parser) {
        return parser::parse;
    }

    default ClauseParser<C, N, E, O, P> ignoreInput() {
        return procedure -> parse(procedure).map(node -> p -> node);
    }

    default NodeParser<C, N, E, O, P> map(Function<N, N> mapping) {
        return procedure -> parse(procedure).map(mapping::apply);
    }

    default NodeParser<C, N, E, O, P> expression(ClauseParser.Mandatory<C, N, E, O, P> mandatory) {
        return procedure -> parse(procedure).map(node -> mandatory.parse(procedure).makeExpression(node));
    }

    default NodeParser<C, N, E, O, P> recursive(ClauseParser<C, N, E, O, P> clauseParser) {
        return procedure -> parse(procedure).map(node -> allOptional(() -> clauseParser.parse(procedure)).stream()
                .reduce(node, (n, clause) -> clause.makeExpression(n), FunctionUtil.notAllowParallelReduce()));
    }

    interface Mandatory<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> extends ParserPx.Mandatory<C, N, E, O, P, N>,
            Parser.Mandatory<C, N, E, O, P, NodeParser<C, N, E, O, P>, NodeParser.Mandatory<C, N, E, O, P>, N> {

        @Override
        default NodeParser<C, N, E, O, P> castParser(Parser<C, N, E, O, P, NodeParser<C, N, E, O, P>,
                Mandatory<C, N, E, O, P>, N> parser) {
            return parser::parse;
        }

        @Override
        default Mandatory<C, N, E, O, P> castMandatory(Parser.Mandatory<C, N, E, O, P, NodeParser<C, N, E, O, P>,
                Mandatory<C, N, E, O, P>, N> mandatory) {
            return mandatory::parse;
        }

        default Mandatory<C, N, E, O, P> map(Function<N, N> mapping) {
            return procedure -> mapping.apply(parse(procedure));
        }

        default Mandatory<C, N, E, O, P> expression(ClauseParser.Mandatory<C, N, E, O, P> expressionClauseMandatory) {
            return procedure -> {
                N node = parse(procedure);
                return expressionClauseMandatory.parse(procedure).makeExpression(node);
            };
        }

        default Mandatory<C, N, E, O, P> concat(ClauseParser<C, N, E, O, P> clauseParser) {
            return procedure -> {
                N node = parse(procedure);
                return clauseParser.parse(procedure).map(right -> right.makeExpression(node)).orElse(node);
            };
        }

        default Mandatory<C, N, E, O, P> recursive(ClauseParser<C, N, E, O, P> clauseParser) {
            return procedure -> {
                N node = parse(procedure);
                Optional<Clause<C, N>> optionalNode = clauseParser.parse(procedure);
                while (optionalNode.isPresent()) {
                    node = optionalNode.get().makeExpression(node);
                    optionalNode = clauseParser.parse(procedure);
                }
                return node;
            };
        }
    }
}
