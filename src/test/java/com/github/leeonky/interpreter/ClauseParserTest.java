package com.github.leeonky.interpreter;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.function.Function;

import static com.github.leeonky.interpreter.ClauseParser.Mandatory.clause;
import static com.github.leeonky.interpreter.Parser.lazyClause;
import static java.util.Optional.empty;
import static java.util.Optional.of;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class ClauseParserTest extends BaseTest {

    @Test
    void lazy_parse() {
        ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser =
                mock(ClauseParser.class);
        TestProcedure procedure = givenProcedureWithCode("");

        Clause<TestContext, TestNode> clause = mock(Clause.class);

        when(clauseParser.parse(procedure)).thenReturn(of(clause));


        assertThat(lazyClause(() -> clauseParser).parse(procedure).get()).isSameAs(clause);
    }

    @Nested
    class Concat {

        @Test
        void concat_with_another_present_clause_parser_should_be_present_clause_parser() {
            TestProcedure givenProcedure = givenProcedureWithCode("");
            TestNode inputNode = new TestNode();
            TestNode firstClauseExpression = new TestNode();
            TestNode lastExpression = new TestNode();

            ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
                assertThat(procedure).isSameAs(givenProcedure);
                Clause<TestContext, TestNode> firstClause = mock(Clause.class);
                when(firstClause.expression(inputNode)).thenReturn(firstClauseExpression);
                return of(firstClause);
            };

            ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser2 = procedure -> {
                assertThat(procedure).isSameAs(givenProcedure);
                Clause<TestContext, TestNode> nextClause = mock(Clause.class);
                when(nextClause.expression(firstClauseExpression)).thenReturn(lastExpression);
                return of(nextClause);
            };

            assertThat(clauseParser.concat(clauseParser2).parse(givenProcedure).get().expression(inputNode)).isSameAs(lastExpression);
        }

        @Test
        void concat_with_another_empty_clause_parser_should_return_first_part_clause_parser() {
            TestProcedure givenProcedure = givenProcedureWithCode("");

            Clause<TestContext, TestNode> clause = mock(Clause.class);

            ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
                assertThat(procedure).isSameAs(givenProcedure);
                return of(clause);
            };

            ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser2 = procedure -> {
                assertThat(procedure).isSameAs(givenProcedure);
                return empty();
            };

            assertThat(clauseParser.concat(clauseParser2).parse(givenProcedure).get()).isSameAs(clause);
        }

        @Test
        void empty_concat_with_any_clause_parser_should_return_empty() {
            TestProcedure testProcedure = givenProcedureWithCode("");
            ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                return empty();
            };

            assertThat(clauseParser.concat(mock(ClauseParser.class)).parse(testProcedure)).isEmpty();
        }
    }

    @Test
    void convert_to_mandatory() {
        TestProcedure testProcedure = givenProcedureWithCode("");
        TestNode node = new TestNode();
        TestNode expression = new TestNode();

        ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
            assertThat(procedure).isSameAs(testProcedure);
            Clause<TestContext, TestNode> clause = mock(Clause.class);
            when(clause.expression(node)).thenReturn(expression);
            return of(clause);
        };

        assertThat(clauseParser.mandatory("").parse(testProcedure).expression(node)).isSameAs(expression);
    }

    @Test
    void convert_to_mandatory_with_mandatory() {
        TestProcedure testProcedure = givenProcedureWithCode("");

        Clause<TestContext, TestNode> clause = mock(Clause.class);
        TestNode node = new TestNode();

        TestNode expression = new TestNode();
        when(clause.expression(node)).thenReturn(expression);

        ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
            assertThat(procedure).isSameAs(testProcedure);
            return empty();
        };

        ClauseParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> mandatory = procedure -> {
            assertThat(procedure).isSameAs(testProcedure);
            return clause;
        };

        assertThat(clauseParser.or(mandatory).parse(testProcedure).expression(node)).isSameAs(expression);
    }

    @Test
    void create_clause_mandatory_with_mandatory() {
        TestProcedure testProcedure = givenProcedureWithCode("");
        TestNode node = new TestNode();
        NodeParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeMandatory = procedure -> {
            assertThat(procedure).isSameAs(testProcedure);
            return node;
        };

        TestNode input = new TestNode();
        Function<TestNode, NodeParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure>> function = inputNode -> {
            assertThat(inputNode).isSameAs(input);
            return nodeMandatory;
        };

        Clause<TestContext, TestNode> clause = clause(function).parse(testProcedure);

        assertThat(clause.expression(input)).isSameAs(node);
    }
}