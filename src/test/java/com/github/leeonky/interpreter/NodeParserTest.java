package com.github.leeonky.interpreter;

import org.junit.jupiter.api.Test;

import java.util.Optional;
import java.util.function.BiFunction;

import static com.github.leeonky.interpreter.NodeParser.lazy;
import static java.util.Optional.empty;
import static java.util.Optional.of;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class NodeParserTest extends BaseTest {

    @Test
    void lazy_parse() {
        NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser =
                mock(NodeParser.class);
        TestNode node = new TestNode();
        TestProcedure procedure = givenProcedureWithCode("");
        when(nodeParser.parse(procedure)).thenReturn(of(node));


        assertThat(lazy(() -> nodeParser).parse(procedure).get()).isSameAs(node);
    }

    @Test
    void parse_ignore_input() {
        TestProcedure testProcedure = givenProcedureWithCode("");
        TestNode node = new TestNode();
        NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
            assertThat(procedure).isSameAs(testProcedure);
            return of(node);
        };

        assertThat(nodeParser.ignoreInput().parse(testProcedure).get().expression(null)).isSameAs(node);
    }

    @Test
    void parse_empty_in_ignore_input() {
        TestProcedure testProcedure = givenProcedureWithCode("");
        NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
            assertThat(procedure).isSameAs(testProcedure);
            return empty();
        };

        assertThat(nodeParser.ignoreInput().parse(testProcedure)).isEmpty();
    }

    @Test
    void parse_clause_parser() {
        TestProcedure testProcedure = givenProcedureWithCode("");
        TestNode node = new TestNode();
        NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
            assertThat(procedure).isSameAs(testProcedure);
            return of(node);
        };

        BiFunction biFunction = mock(BiFunction.class);
        TestNode input = new TestNode();
        TestNode expression = new TestNode();
        when(biFunction.apply(input, node)).thenReturn(expression);

        Optional<Clause> optional = nodeParser.clauseParser(biFunction).parse(testProcedure);

        assertThat(optional.get().expression(input)).isSameAs(expression);
    }

    @Test
    void parse_empty_in_clause_parser() {
        TestProcedure testProcedure = givenProcedureWithCode("");

        NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
            assertThat(procedure).isSameAs(testProcedure);
            return empty();
        };

        assertThat(nodeParser.clauseParser(null).parse(testProcedure)).isEmpty();
    }

    @Test
    void parse_expression() {
        TestProcedure testProcedure = givenProcedureWithCode("");
        TestNode node = new TestNode();
        NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
            assertThat(procedure).isSameAs(testProcedure);
            return of(node);
        };

        Clause<TestContext, TestNode> clause = mock(Clause.class);
        TestNode expression = new TestNode();
        when(clause.expression(node)).thenReturn(expression);
        ClauseParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> mandatory = procedure -> {
            assertThat(procedure).isSameAs(testProcedure);
            return clause;
        };

        assertThat(nodeParser.expression(mandatory).parse(testProcedure).get()).isSameAs(expression);
    }

    @Test
    void parse_empty_expression() {
        TestProcedure testProcedure = givenProcedureWithCode("");

        NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
            assertThat(procedure).isSameAs(testProcedure);
            return empty();
        };

        assertThat(nodeParser.expression(null).parse(testProcedure)).isEmpty();
    }
}
