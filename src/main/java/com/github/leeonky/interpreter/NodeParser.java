package com.github.leeonky.interpreter;

import java.util.Optional;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Stream;

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
    Optional<N> parse(P procedure);

    default Mandatory<C, N, E, O, P> or(Mandatory<C, N, E, O, P> nodeFactory) {
        return procedure -> parse(procedure).orElseGet(() -> nodeFactory.parse(procedure));
    }

    @Override
    default Mandatory<C, N, E, O, P> mandatory(String message) {
        return procedure -> parse(procedure).orElseThrow(() -> procedure.getSourceCode().syntaxError(message, 0));
    }

    default ClauseParser<C, N, E, O, P> clause() {
        return procedure -> parse(procedure).map(node -> p -> node);
    }

    default ClauseParser<C, N, E, O, P> clause(Supplier<O> operator) {
        return procedure -> parse(procedure).map(node -> p -> procedure.createExpression(p, operator.get()
                .setPosition(node.getPositionBegin()), node));
    }

    @Override
    default Mandatory<C, N, E, O, P> cast(Parser.Mandatory<C, N, E, O, P, NodeParser<C, N, E, O, P>,
            NodeParser.Mandatory<C, N, E, O, P>, N> mandatory) {
        return mandatory::parse;
    }

    interface Mandatory<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> extends ParserPx.Mandatory<C, N, E, O, P, N>,
            Parser.Mandatory<C, N, E, O, P, NodeParser<C, N, E, O, P>, NodeParser.Mandatory<C, N, E, O, P>, N> {

        @Override
        N parse(P procedure);

        @Deprecated
        default Mandatory<C, N, E, O, P> map(Function<N, N> mapping) {
            return procedure -> mapping.apply(parse(procedure));
        }

        default Mandatory<C, N, E, O, P> node(ClauseParser.Mandatory<C, N, E, O, P> expressionClauseMandatory) {
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

        default Mandatory<C, N, E, O, P> recursiveConcat(ClauseParser<C, N, E, O, P> clauseParser) {
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

        @Override
        default NodeParser<C, N, E, O, P> cast(Parser<C, N, E, O, P, NodeParser<C, N, E, O, P>, Mandatory<C, N, E, O, P>, N> parser) {
            return parser::parse;
        }
    }
}
