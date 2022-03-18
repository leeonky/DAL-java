package com.github.leeonky.interpreter;

import org.junit.jupiter.api.Test;

import static com.github.leeonky.interpreter.ClauseParser.lazy;
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
}