package com.github.leeonky.interpreter;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.Optional;

import static com.github.leeonky.interpreter.Notation.notation;
import static com.github.leeonky.interpreter.Parser.oneOf;
import static java.util.Collections.emptyMap;
import static java.util.Optional.empty;
import static java.util.Optional.of;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.fail;
import static org.junit.jupiter.api.Assertions.assertThrows;

class OperatorParserTest extends BaseTest {

    @Nested
    class Parser {

        @Nested
        class OneOf {

            @Test
            void return_empty_when_all_parse_empty() {
                OperatorParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure>
                        emptyParser1 = procedure -> empty(),
                        emptyParser2 = procedure -> empty();

                assertThat(oneOf(emptyParser1).parse(givenProcedureWithCode(""))).isEmpty();

                assertThat(oneOf(emptyParser1, emptyParser2).parse(givenProcedureWithCode(""))).isEmpty();
            }

            @Test
            void return_operator_when_matches() {
                TestOperator testOperator = new TestOperator();
                OperatorParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure>
                        operatorParser = procedure -> of(testOperator),
                        emptyParser = procedure -> empty();

                assertThat(oneOf(operatorParser).parse(givenProcedureWithCode(""))).hasValue(testOperator);

                assertThat(oneOf(emptyParser, operatorParser).parse(givenProcedureWithCode(""))).hasValue(testOperator);

                assertThat(oneOf(emptyParser, operatorParser, procedure -> {
                    fail("");
                    return empty();
                }).parse(givenProcedureWithCode(""))).hasValue(testOperator);
            }
        }

        @Nested
        class CombineWithMandatory {

            @Test
            void should_parser_result_when_present() {
                TestOperator testOperator = new TestOperator();
                OperatorParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure>
                        operatorParser = procedure -> of(testOperator);

                OperatorParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure>
                        operatorParserMandatory = procedure -> {
                    fail("");
                    return null;
                };

                assertThat(operatorParser.or(operatorParserMandatory).parse(givenProcedureWithCode(""))).isSameAs(testOperator);
            }

            @Test
            void should_return_mandatory_result_when_parser_is_empty() {
                TestOperator testOperator = new TestOperator();
                OperatorParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure>
                        emptyParser = procedure -> empty();
                OperatorParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure>
                        operatorParserMandatory = procedure -> testOperator;

                assertThat(emptyParser.or(operatorParserMandatory).parse(givenProcedureWithCode(""))).isSameAs(testOperator);
            }
        }

        @Nested
        class Mandatory {

            @Test
            void raise_error_when_not_match() {
                OperatorParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure>
                        operatorParser = procedure -> empty();

                SyntaxException syntaxException = assertThrows(SyntaxException.class, () ->
                        operatorParser.mandatory("raise error").parse(givenProcedureWithCode("given code")));

                assertThat(syntaxException.show("given code")).isEqualTo("given code\n^");
            }

            @Test
            void return_when_matches() {
                TestOperator testOperator = new TestOperator();
                OperatorParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure>
                        operatorParser = procedure -> of(testOperator);

                assertThat(operatorParser.mandatory("").parse(givenProcedureWithCode(""))).isSameAs(testOperator);
            }
        }

        @Nested
        class ToClause {

            @Nested
            class WithMandatory {

                @Test
                void return_empty_when_no_matches_operator() {
                    OperatorParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure>
                            operatorParser = procedure -> empty();

                    NodeParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure>
                            nodeMandatory = procedure -> null;

                    assertThat(operatorParser.clause(nodeMandatory).parse(givenProcedureWithCode(""))).isEmpty();
                }

                @Test
                void return_clause_with_operator_and_node() {
                    TestOperator testOperator = new TestOperator();

                    OperatorParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure>
                            operatorParser = procedure -> of(testOperator);

                    TestNode testNode = new TestNode();

                    NodeParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure>
                            nodeMandatory = procedure -> {
                        assertThat(procedure.currentOperator()).hasValue(testOperator);
                        return testNode;
                    };

                    TestProcedure procedure = new TestProcedure(BaseTest.createSourceCode(""), TestExpression::new);
                    Optional<Clause<TestContext, TestNode>> clause =
                            operatorParser.clause(nodeMandatory).parse(procedure);

                    TestExpression expression = (TestExpression) clause.get().expression(new TestNode());

                    assertThat(expression.getOperator()).isSameAs(testOperator);
                    assertThat(expression.getRightOperand()).isSameAs(testNode);
                }
            }

            @Nested
            class WithParser {

                @Test
                void return_empty_when_no_matches_operator() {
                    OperatorParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure>
                            operatorParser = procedure -> empty();

                    NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure>
                            nodeParser = procedure -> of(new TestNode());

                    assertThat(operatorParser.clause(nodeParser).parse(givenProcedureWithCode(""))).isEmpty();
                }

                @Test
                void return_empty_when_node_parser_return_empty() {
                    OperatorParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure>
                            operatorParser = notation("=").operator(TestOperator::new);

                    NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure>
                            emptyParser = procedure -> empty();

                    TestProcedure procedure = givenProcedureWithCode("=");

                    assertThat(operatorParser.clause(emptyParser).parse(procedure)).isEmpty();
                    assertThat(procedure.getSourceCode().popChar(emptyMap())).isEqualTo('=');
                }

                @Test
                void return_clause_with_operator_and_node() {
                    TestOperator testOperator = new TestOperator();

                    OperatorParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure>
                            operatorParser = procedure -> of(testOperator);

                    TestNode testNode = new TestNode();

                    NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure>
                            nodeParser = procedure -> {
                        assertThat(procedure.currentOperator()).hasValue(testOperator);
                        return of(testNode);
                    };

                    TestProcedure procedure = new TestProcedure(BaseTest.createSourceCode(""), TestExpression::new);
                    Optional<Clause<TestContext, TestNode>> clause =
                            operatorParser.clause(nodeParser).parse(procedure);

                    TestExpression expression = (TestExpression) clause.get().expression(new TestNode());

                    assertThat(expression.getOperator()).isSameAs(testOperator);
                    assertThat(expression.getRightOperand()).isSameAs(testNode);
                }
            }
        }

        @Nested
        class Unary {

            @Nested
            class WithMandatory {

                @Test
                void return_empty_when_no_matches_operator() {
                    OperatorParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure>
                            operatorParser = procedure -> empty();

                    NodeParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure>
                            nodeMandatory = procedure -> null;

                    assertThat(operatorParser.unary(nodeMandatory).parse(givenProcedureWithCode(""))).isEmpty();
                }

                @Test
                void return_clause_with_operator_and_node() {
                    TestOperator testOperator = new TestOperator();

                    OperatorParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure>
                            operatorParser = procedure -> of(testOperator);

                    TestNode testNode = new TestNode();

                    NodeParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure>
                            nodeMandatory = procedure -> {
                        assertThat(procedure.currentOperator()).hasValue(testOperator);
                        return testNode;
                    };

                    TestProcedure procedure = new TestProcedure(BaseTest.createSourceCode(""), TestExpression::new);

                    TestExpression expression = (TestExpression) operatorParser.unary(nodeMandatory).parse(procedure).get();

                    assertThat(expression.getOperator()).isSameAs(testOperator);
                    assertThat(expression.getRightOperand()).isSameAs(testNode);
                }
            }
        }
    }

    @Nested
    class Mandatory {

        @Nested
        class ToClause {

            @Nested
            class WithMandatory {

                @Test
                void return_clause_with_operator_and_node() {
                    TestOperator testOperator = new TestOperator();

                    OperatorParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure>
                            operatorMandatory = procedure -> testOperator;

                    TestNode testNode = new TestNode();

                    NodeParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure>
                            nodeMandatory = procedure -> {
                        assertThat(procedure.currentOperator()).hasValue(testOperator);
                        return testNode;
                    };

                    TestProcedure procedure = new TestProcedure(BaseTest.createSourceCode(""), TestExpression::new);
                    Clause<TestContext, TestNode> clause =
                            operatorMandatory.clause(nodeMandatory).parse(procedure);

                    TestExpression expression = (TestExpression) clause.expression(new TestNode());

                    assertThat(expression.getOperator()).isSameAs(testOperator);
                    assertThat(expression.getRightOperand()).isSameAs(testNode);
                }
            }
        }
    }
}