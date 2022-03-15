package com.github.leeonky.interpreter;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;

import static com.github.leeonky.interpreter.NodeParser.lazy;
import static java.util.Collections.emptyMap;
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

    @Nested
    class Mandatory {

        @Test
        void map() {
            TestProcedure testProcedure = givenProcedureWithCode("");
            TestNode node = new TestNode();
            NodeParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeMandatory = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                return node;
            };

            Function<TestNode, TestNode> mapper = mock(Function.class);
            TestNode mappedNode = new TestNode();
            when(mapper.apply(node)).thenReturn(mappedNode);

            assertThat(nodeMandatory.map(mapper).parse(testProcedure)).isSameAs(mappedNode);
        }

        @Test
        void expression() {
            TestProcedure testProcedure = givenProcedureWithCode("");
            TestNode node = new TestNode();
            NodeParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeMandatory = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                return node;
            };

            Clause<TestContext, TestNode> clause = mock(Clause.class);
            TestNode expression = new TestNode();
            when(clause.expression(node)).thenReturn(expression);
            ClauseParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> mandatory = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                return clause;
            };

            assertThat(nodeMandatory.expression(mandatory).parse(testProcedure)).isSameAs(expression);
        }

        @Test
        void combine_with_clause_parser() {
            TestProcedure testProcedure = givenProcedureWithCode("");
            TestNode node = new TestNode();
            NodeParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeMandatory = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                return node;
            };

            Clause<TestContext, TestNode> clause = mock(Clause.class);
            TestNode expression = new TestNode();
            when(clause.expression(node)).thenReturn(expression);
            ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                return of(clause);
            };

            assertThat(nodeMandatory.combine(clauseParser).parse(testProcedure).get()).isSameAs(expression);
        }

        @Test
        void combine_with_empty_clause_parser() {
            TestProcedure testProcedure = givenProcedureWithCode("a");
            TestNode node = new TestNode();
            NodeParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeMandatory = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                testProcedure.getSourceCode().popChar(emptyMap());
                return node;
            };

            ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                return empty();
            };

            assertThat(nodeMandatory.combine(clauseParser).parse(testProcedure)).isEmpty();
            assertThat(testProcedure.getSourceCode().nextPosition()).isEqualTo(0);
        }
    }
}
