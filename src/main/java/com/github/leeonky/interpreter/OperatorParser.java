package com.github.leeonky.interpreter;

public interface OperatorParser<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> extends Parser<C, N, E, O, P,
        OperatorParser<C, N, E, O, P>, OperatorParser.Mandatory<C, N, E, O, P>, O> {

    @Override
    default Mandatory<C, N, E, O, P> castMandatory(Parser.Mandatory<C, N, E, O, P, OperatorParser<C, N, E, O, P>,
            Mandatory<C, N, E, O, P>, O> mandatory) {
        return mandatory::parse;
    }

    @Override
    default OperatorParser<C, N, E, O, P> castParser(Parser<C, N, E, O, P, OperatorParser<C, N, E, O, P>,
            Mandatory<C, N, E, O, P>, O> parser) {
        return parser::parse;
    }

    default ClauseParser<C, N, E, O, P> clause(NodeParser.Mandatory<C, N, E, O, P> nodeFactory) {
        return procedure -> parse(procedure).map(operator -> procedure.underOperator(operator, () -> {
            N right = nodeFactory.parse(procedure);
            return left -> procedure.createExpression(left, operator, right);
        }));
    }

    default ClauseParser<C, N, E, O, P> clause(NodeParser<C, N, E, O, P> nodeParser) {
        return procedure -> procedure.getSourceCode().tryFetch(() -> parse(procedure).map(operator ->
                procedure.underOperator(operator, () -> nodeParser.parse(procedure).<Clause<C, N>>map(n ->
                        left -> procedure.createExpression(left, operator, n)).orElse(null))));
    }

    default NodeParser<C, N, E, O, P> unary(NodeParser.Mandatory<C, N, E, O, P> nodeFactory) {
        return procedure -> parse(procedure).map(operator -> procedure.underOperator(operator, () ->
                procedure.createExpression(null, operator, nodeFactory.parse(procedure))));
    }

    interface Mandatory<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>> extends Parser.Mandatory<C, N, E, O, P,
            OperatorParser<C, N, E, O, P>, OperatorParser.Mandatory<C, N, E, O, P>, O> {

        @Override
        default OperatorParser<C, N, E, O, P> castParser(Parser<C, N, E, O, P, OperatorParser<C, N, E, O, P>,
                Mandatory<C, N, E, O, P>, O> parser) {
            return parser::parse;
        }

        default ClauseParser.Mandatory<C, N, E, O, P> clause(NodeParser.Mandatory<C, N, E, O, P> nodeFactory) {
            return procedure -> {
                O operator = parse(procedure);
                return procedure.underOperator(operator, () -> {
                    N right = nodeFactory.parse(procedure);
                    return left -> procedure.createExpression(left, operator, right);
                });
            };
        }
    }
}
