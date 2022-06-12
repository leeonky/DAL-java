package com.github.leeonky.interpreter;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Optional;
import java.util.function.BiFunction;

import static com.github.leeonky.interpreter.NodeParser.positionNode;
import static com.github.leeonky.interpreter.Parser.lazyNode;
import static java.util.Collections.emptyMap;
import static java.util.Optional.empty;
import static java.util.Optional.of;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.*;

class NodeParserTest extends BaseTest {

    @Nested
    class SavePosition {

        @Test
        void should_save_position_when_present_node() {
            TestProcedure testProcedure = givenProcedureWithCode("ab");

            NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> charNode = procedure ->
                    of(new TestNode(procedure.getSourceCode().popChar(new HashMap<>())));

            assertThat(positionNode(charNode).parse(testProcedure).get().getPositionBegin()).isEqualTo(0);
            assertThat(positionNode(charNode).parse(testProcedure).get().getPositionBegin()).isEqualTo(1);
        }

        @Test
        void should_save_position() {
            TestProcedure testProcedure = givenProcedureWithCode("ab");

            NodeParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> charNode = procedure ->
                    new TestNode(procedure.getSourceCode().popChar(new HashMap<>()));

            assertThat(positionNode(charNode).parse(testProcedure).getPositionBegin()).isEqualTo(0);
            assertThat(positionNode(charNode).parse(testProcedure).getPositionBegin()).isEqualTo(1);
        }
    }

    @Nested
    class Concat {

        @Nested
        class MandatoryClauseParser {

            @Test
            void present_node_parser_concat_mandatory_clause_parser_should_be_present_node_parser_which_can_parse_expression_with_node_and_clause_by_given_parser() {
                TestProcedure testProcedure = givenProcedureWithCode("");
                TestNode node = new TestNode();
                NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    return of(node);
                };

                TestNode expression = new TestNode();
                ClauseParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> mandatory = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    Clause<TestContext, TestNode> clause = mock(Clause.class);
                    when(clause.expression(node)).thenReturn(expression);
                    return clause;
                };

                assertThat(nodeParser.concat(mandatory).parse(testProcedure).get()).isSameAs(expression);
            }

            @Test
            void empty_node_parser_concat_any_mandatory_clause_parser_should_be_empty_node_parser() {
                TestProcedure testProcedure = givenProcedureWithCode("");

                NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    return empty();
                };

                assertThat(nodeParser.concat((ClauseParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator,
                        TestProcedure>) null).parse(testProcedure)).isEmpty();
            }
        }

        @Nested
        class ConcatClauseParser {

            @Test
            void present_node_parser_concat_present_clause_parser_should_be_present_node_parser_which_can_parse_expression_with_node_and_clause() {
                TestProcedure testProcedure = givenProcedureWithCode("");
                TestNode node = new TestNode();
                NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    return of(node);
                };

                TestNode expression = new TestNode();
                ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    Clause<TestContext, TestNode> clause = mock(Clause.class);
                    when(clause.expression(node)).thenReturn(expression);
                    return of(clause);
                };

                assertThat(nodeParser.concat(clauseParser).parse(testProcedure).get()).isSameAs(expression);
            }

            @Test
            void empty_node_parser_concat_any_present_clause_parser_should_be_empty_node_parser() {
                TestProcedure testProcedure = givenProcedureWithCode("");
                NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> empty();
                ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = mock(ClauseParser.class);

                assertThat(nodeParser.concat(clauseParser).parse(testProcedure)).isEmpty();
                verify(clauseParser, never()).parse(testProcedure);
            }

            @Test
            void present_node_parser_concat_empty_clause_parser_should_return_present_node_parser_which_can_parse_node_by_given_node_parser() {
                TestProcedure testProcedure = givenProcedureWithCode("");
                TestNode node = new TestNode();
                NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    return of(node);
                };

                ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    return empty();
                };

                assertThat(nodeParser.concat(clauseParser).parse(testProcedure).get()).isSameAs(node);
            }
        }
    }

    @Nested
    class With {

        @Nested
        class WithClauseParser {

            @Test
            void present_node_parser_with_present_clause_parser_should_be_present_node_parser_which_can_parse_expression_with_node_and_clause_by_given_parser() {
                TestProcedure testProcedure = givenProcedureWithCode("");
                TestNode node = new TestNode();
                NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    return of(node);
                };

                TestNode expression = new TestNode();
                ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    Clause<TestContext, TestNode> clause = mock(Clause.class);
                    when(clause.expression(node)).thenReturn(expression);
                    return of(clause);
                };

                assertThat(nodeParser.with(clauseParser).parse(testProcedure).get()).isSameAs(expression);
            }

            @Test
            void present_node_parser_with_empty_clause_parser_should_be_empty_node_parser_and_do_not_change_code_position() {
                TestProcedure testProcedure = givenProcedureWithCode("a");
                NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    procedure.getSourceCode().popChar(new HashMap<>());
                    return of(new TestNode());
                };

                ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    return empty();
                };

                assertThat(nodeParser.with(clauseParser).parse(testProcedure)).isEmpty();
                assertThat(testProcedure.getSourceCode().nextPosition()).isEqualTo(0);
            }

            @Test
            void empty_node_parser_with_empty_clause_parser_should_be_empty_node_parser_and_do_not_change_code_position() {
                TestProcedure testProcedure = givenProcedureWithCode("a");
                NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    return empty();
                };

                ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    return empty();
                };

                assertThat(nodeParser.with(clauseParser).parse(testProcedure)).isEmpty();
                assertThat(testProcedure.getSourceCode().nextPosition()).isEqualTo(0);
            }
        }

        @Nested
        class WithMandatoryClauseParser {

            @Test
            void present_node_parser_with_mandatory_clause_parser_should_be_present_node_parser_which_can_parse_expression_with_node_and_clause_by_given_parser() {
                TestProcedure testProcedure = givenProcedureWithCode("");
                TestNode node = new TestNode();
                NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    return of(node);
                };

                TestNode expression = new TestNode();
                ClauseParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> mandatory = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    Clause<TestContext, TestNode> clause = mock(Clause.class);
                    when(clause.expression(node)).thenReturn(expression);
                    return clause;
                };

                assertThat(nodeParser.with(mandatory).parse(testProcedure).get()).isSameAs(expression);
            }

            @Test
            void empty_node_parser_with_any_clause_parser_should_be_empty_node_parser_and_do_not_change_code_position() {
                TestProcedure testProcedure = givenProcedureWithCode("a");
                NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    return empty();
                };

                assertThat(nodeParser.with((ClauseParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure>) null).parse(testProcedure)).isEmpty();
                assertThat(testProcedure.getSourceCode().nextPosition()).isEqualTo(0);
            }
        }
    }

    @Test
    void lazy_parse() {
        NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser =
                mock(NodeParser.class);
        TestNode node = new TestNode();
        TestProcedure procedure = givenProcedureWithCode("");
        when(nodeParser.parse(procedure)).thenReturn(of(node));


        assertThat(lazyNode(() -> nodeParser).parse(procedure).get()).isSameAs(node);
    }

    @Nested
    class ConcatAll {

        @Test
        void empty_node_parser_concat_all_any_clause_parser_should_be_empty_node_parser() {
            TestProcedure testProcedure = givenProcedureWithCode("");
            NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                return empty();
            };

            ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = mock(ClauseParser.class);

            assertThat(nodeParser.concatAll(clauseParser).parse(testProcedure)).isEmpty();
        }

        @Test
        void present_node_parser_concat_all_empty_clause_parser_should_return_present_node_parser_which_can_parse_node_by_given_node_parser() {
            TestProcedure testProcedure = givenProcedureWithCode("");
            TestNode node = new TestNode();
            NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                return of(node);
            };

            ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                return empty();
            };

            assertThat(nodeParser.concatAll(clauseParser).parse(testProcedure).get()).isSameAs(node);
        }

        int recursiveTimes = 0;

        @Test
        void present_node_parser_concat_all_present_clause_parser_which_has_one_clause_should_return_present_node_parser_which_can_parse_expression_with_node_and_one_clause() {
            recursiveTimes = 1;
            TestProcedure testProcedure = givenProcedureWithCode("");
            TestNode node = new TestNode();
            NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                return of(node);
            };

            TestNode expression = new TestNode();
            ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                if (recursiveTimes-- > 0) {
                    Clause<TestContext, TestNode> clause = mock(Clause.class);
                    when(clause.expression(node)).thenReturn(expression);
                    return of(clause);
                }
                return empty();
            };

            assertThat(nodeParser.concatAll(clauseParser).parse(testProcedure).get()).isSameAs(expression);
        }

        @Test
        void present_node_parser_concat_all_present_clause_parser_which_has_two_clauses_should_return_present_node_parser_which_can_parse_expression_with_node_and_two_clause() {
            recursiveTimes = 2;
            TestProcedure testProcedure = givenProcedureWithCode("");
            TestNode node = new TestNode();
            NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                return of(node);
            };

            TestNode expression = new TestNode();
            TestNode lastExpression = new TestNode();
            ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                switch (recursiveTimes--) {
                    case 2:
                        Clause<TestContext, TestNode> clause = mock(Clause.class);
                        when(clause.expression(node)).thenReturn(expression);
                        return of(clause);
                    case 1:
                        Clause<TestContext, TestNode> clause2 = mock(Clause.class);
                        when(clause2.expression(expression)).thenReturn(lastExpression);
                        return of(clause2);
                    default:
                        return empty();
                }
            };

            assertThat(nodeParser.concatAll(clauseParser).parse(testProcedure).get()).isSameAs(lastExpression);
        }
    }

    @Nested
    class ToClause {

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

            Optional<Clause> optional = nodeParser.clause(biFunction).parse(testProcedure);


            assertThat(optional.get().expression(input)).isSameAs(expression);


        }

        @Test
        void parse_empty_in_clause_parser() {
            TestProcedure testProcedure = givenProcedureWithCode("");

            NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                return empty();
            };

            assertThat(nodeParser.clause(null).parse(testProcedure)).isEmpty();
        }
    }

    @Nested
    class Mandatory {

        @Nested
        class ConcatClauseParser {

            @Test
            void concat_present_clause_parser_should_be_present_node_parser_which_can_parse_expression_with_node_and_clause() {
                TestProcedure testProcedure = givenProcedureWithCode("");
                TestNode node = new TestNode();
                NodeParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeMandatory = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    return node;
                };

                TestNode expression = new TestNode();
                ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    Clause<TestContext, TestNode> clause = mock(Clause.class);
                    when(clause.expression(node)).thenReturn(expression);
                    return of(clause);
                };

                assertThat(nodeMandatory.concat(clauseParser).parse(testProcedure)).isSameAs(expression);
            }

            @Test
            void concat_empty_clause_parser_should_return_present_node_parser_which_can_parse_node_by_given_node_parser() {
                TestProcedure testProcedure = givenProcedureWithCode("");
                TestNode node = new TestNode();
                NodeParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeMandatory = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    return node;
                };

                ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    return empty();
                };

                assertThat(nodeMandatory.concat(clauseParser).parse(testProcedure)).isSameAs(node);
            }
        }

        @Nested
        class MandatoryClauseParser {

            @Test
            void should_be_mondatory_which_can_parse_expression_with_node_and_clause() {
                TestProcedure testProcedure = givenProcedureWithCode("");
                TestNode node = new TestNode();
                NodeParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeMandatory = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    return node;
                };

                TestNode expression = new TestNode();
                ClauseParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> mandatory = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    Clause<TestContext, TestNode> clause = mock(Clause.class);
                    when(clause.expression(node)).thenReturn(expression);
                    return clause;
                };

                assertThat(nodeMandatory.concat(mandatory).parse(testProcedure)).isSameAs(expression);
            }
        }

        @Nested
        class With {

            @Nested
            class WithClauseParser {

                @Test
                void with_present_clause_parser_should_be_present_node_parser_which_can_parse_expression_with_node_and_clause_by_given_parser() {
                    TestProcedure testProcedure = givenProcedureWithCode("");
                    TestNode node = new TestNode();
                    NodeParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeMandatory = procedure -> {
                        assertThat(procedure).isSameAs(testProcedure);
                        return node;
                    };

                    TestNode expression = new TestNode();
                    ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
                        assertThat(procedure).isSameAs(testProcedure);
                        Clause<TestContext, TestNode> clause = mock(Clause.class);
                        when(clause.expression(node)).thenReturn(expression);
                        return of(clause);
                    };

                    assertThat(nodeMandatory.with(clauseParser).parse(testProcedure).get()).isSameAs(expression);
                }

                @Test
                void with_empty_clause_parser_should_be_empty_node_parser_and_do_not_change_code_position() {
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

                    assertThat(nodeMandatory.with(clauseParser).parse(testProcedure)).isEmpty();
                    assertThat(testProcedure.getSourceCode().nextPosition()).isEqualTo(0);
                }
            }

            @Nested
            class MandatoryClauseParser {

                @Test
                void with_mandatory_clause_parser_should_be_present_node_parser_which_can_parse_expression_with_node_and_clause_by_given_parser() {
                    TestProcedure testProcedure = givenProcedureWithCode("");
                    TestNode node = new TestNode();
                    NodeParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeMandatory = procedure -> {
                        assertThat(procedure).isSameAs(testProcedure);
                        return node;
                    };

                    TestNode expression = new TestNode();
                    ClauseParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> mandatory = procedure -> {
                        assertThat(procedure).isSameAs(testProcedure);
                        Clause<TestContext, TestNode> clause = mock(Clause.class);
                        when(clause.expression(node)).thenReturn(expression);
                        return clause;
                    };

                    assertThat(nodeMandatory.with(mandatory).parse(testProcedure)).isSameAs(expression);
                }
            }
        }

        @Nested
        class ConcatAll {

            @Test
            void concat_all_empty_clause_parser_should_return_mandatory_node_parser_which_can_parse_node_by_given_node_parser() {
                TestProcedure testProcedure = givenProcedureWithCode("");
                TestNode node = new TestNode();
                NodeParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeMandatory = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    return node;
                };

                ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    return empty();
                };

                assertThat(nodeMandatory.concatAll(clauseParser).parse(testProcedure)).isSameAs(node);
            }

            int recursiveTimes = 0;

            @Test
            void concat_all_present_clause_parser_which_has_one_clause_should_return_mandatory_node_parser_which_can_parse_expression_with_node_and_one_clause() {
                recursiveTimes = 1;
                TestProcedure testProcedure = givenProcedureWithCode("");
                TestNode node = new TestNode();
                NodeParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeMandatory = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    return node;
                };

                TestNode expression = new TestNode();
                ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    if (recursiveTimes-- > 0) {
                        Clause<TestContext, TestNode> clause = mock(Clause.class);
                        when(clause.expression(node)).thenReturn(expression);
                        return of(clause);
                    }
                    return empty();
                };

                assertThat(nodeMandatory.concatAll(clauseParser).parse(testProcedure)).isSameAs(expression);
            }

            @Test
            void concat_all_present_clause_parser_which_has_two_clauses_should_return_mandatory_node_parser_which_can_parse_expression_with_node_and_two_clause() {
                recursiveTimes = 2;
                TestProcedure testProcedure = givenProcedureWithCode("");
                TestNode node = new TestNode();
                NodeParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeMandatory = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    return node;
                };

                TestNode expression = new TestNode();
                TestNode lastExpression = new TestNode();

                ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    switch (recursiveTimes--) {
                        case 2:
                            Clause<TestContext, TestNode> clause = mock(Clause.class);
                            when(clause.expression(node)).thenReturn(expression);
                            return of(clause);
                        case 1:
                            Clause<TestContext, TestNode> clause2 = mock(Clause.class);
                            when(clause2.expression(expression)).thenReturn(lastExpression);
                            return of(clause2);
                        default:
                            return empty();
                    }
                };

                assertThat(nodeMandatory.concatAll(clauseParser).parse(testProcedure)).isSameAs(lastExpression);
            }
        }
    }
}
