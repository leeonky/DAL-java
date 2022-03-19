package com.github.leeonky.interpreter;

import org.junit.jupiter.api.Test;

import static com.github.leeonky.interpreter.ClauseParser.lazy;
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


        assertThat(lazy(() -> clauseParser).parse(procedure).get()).isSameAs(clause);
    }

    @Test
    void concat_with_another_clause_parser() {
        TestProcedure testProcedure = givenProcedureWithCode("");

        Clause<TestContext, TestNode> clause = mock(Clause.class);
        TestNode node = new TestNode();

        TestNode expression = new TestNode();
        when(clause.expression(node)).thenReturn(expression);

        ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
            assertThat(procedure).isSameAs(testProcedure);
            return of(clause);
        };

        Clause<TestContext, TestNode> nextClause = mock(Clause.class);

        TestNode expression2 = new TestNode();
        when(nextClause.expression(expression)).thenReturn(expression2);

        ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser2 = procedure -> {
            assertThat(procedure).isSameAs(testProcedure);
            return of(nextClause);
        };

        assertThat(clauseParser.concat(clauseParser2)
                .parse(testProcedure)
                .get().expression(node)).isSameAs(expression2);
    }

    @Test
    void concat_with_another_empty_clause_parser() {
        TestProcedure testProcedure = givenProcedureWithCode("");

        Clause<TestContext, TestNode> clause = mock(Clause.class);

        ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
            assertThat(procedure).isSameAs(testProcedure);
            return of(clause);
        };

        ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser2 = procedure -> {
            assertThat(procedure).isSameAs(testProcedure);
            return empty();
        };

        assertThat(clauseParser.concat(clauseParser2)
                .parse(testProcedure)
                .get()).isSameAs(clause);
    }

    @Test
    void empty_concat_with_any_clause_parser() {
        TestProcedure testProcedure = givenProcedureWithCode("");
        ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
            assertThat(procedure).isSameAs(testProcedure);
            return empty();
        };

        assertThat(clauseParser.concat(mock(ClauseParser.class))
                .parse(testProcedure)).isEmpty();
    }

    @Test
    void default_input() {
        TestProcedure testProcedure = givenProcedureWithCode("");

        Clause<TestContext, TestNode> clause = mock(Clause.class);
        TestNode node = new TestNode();

        TestNode expression = new TestNode();
        when(clause.expression(node)).thenReturn(expression);

        ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
            assertThat(procedure).isSameAs(testProcedure);
            return of(clause);
        };

        assertThat(clauseParser.defaultInputNode(node).parse(testProcedure).get()).isSameAs(expression);
    }

    @Test
    void empty_default_input() {
        TestProcedure testProcedure = givenProcedureWithCode("");
        ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
            assertThat(procedure).isSameAs(testProcedure);
            return empty();
        };

        assertThat(clauseParser.defaultInputNode(null).parse(testProcedure)).isEmpty();
    }

    @Test
    void mandatory_with_input() {
        TestProcedure testProcedure = givenProcedureWithCode("");

        Clause<TestContext, TestNode> clause = mock(Clause.class);
        TestNode node = new TestNode();

        TestNode expression = new TestNode();
        when(clause.expression(node)).thenReturn(expression);

        ClauseParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> mandatory = procedure -> {
            assertThat(procedure).isSameAs(testProcedure);
            return clause;
        };

        assertThat(mandatory.input(node).parse(testProcedure)).isSameAs(expression);
    }
}