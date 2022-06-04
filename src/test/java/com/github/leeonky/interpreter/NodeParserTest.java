package com.github.leeonky.interpreter;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;

import static com.github.leeonky.interpreter.NodeParser.Mandatory.clause;
import static com.github.leeonky.interpreter.Parser.lazyNode;
import static java.util.Collections.emptyMap;
import static java.util.Optional.empty;
import static java.util.Optional.of;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.*;

class NodeParserTest extends BaseTest {

    @Test
    void lazy_parse() {
        NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser =
                mock(NodeParser.class);
        TestNode node = new TestNode();
        TestProcedure procedure = givenProcedureWithCode("");
        when(nodeParser.parse(procedure)).thenReturn(of(node));


        assertThat(lazyNode(() -> nodeParser).parse(procedure).get()).isSameAs(node);
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

    @Test
    void concat_clause_parser() {
        TestProcedure testProcedure = givenProcedureWithCode("");
        TestNode node = new TestNode();
        NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
            assertThat(procedure).isSameAs(testProcedure);
            return of(node);
        };

        Clause<TestContext, TestNode> clause = mock(Clause.class);
        TestNode expression = new TestNode();
        when(clause.expression(node)).thenReturn(expression);
        ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
            assertThat(procedure).isSameAs(testProcedure);
            return of(clause);
        };

        assertThat(nodeParser.concat(clauseParser).parse(testProcedure).get()).isSameAs(expression);
    }

    @Test
    void empty_concat_clause_parser_should_return_empty() {
        TestProcedure testProcedure = givenProcedureWithCode("");
        NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> empty();
        ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = mock(ClauseParser.class);

        assertThat(nodeParser.concat(clauseParser).parse(testProcedure)).isEmpty();
        verify(clauseParser, never()).parse(testProcedure);
    }

    @Test
    void concat_empty_clause_parser_should_return_self_node() {
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

    @Nested
    class Recursive {

        @Test
        void empty_recursive_clause_parser() {
            TestProcedure testProcedure = givenProcedureWithCode("");
            NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                return empty();
            };

            ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = mock(ClauseParser.class);

            assertThat(nodeParser.recursive(clauseParser).parse(testProcedure)).isEmpty();
        }

        @Test
        void recursive_with_empty_clause_parser() {
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

            assertThat(nodeParser.recursive(clauseParser).parse(testProcedure).get()).isSameAs(node);
        }

        int recursiveTimes = 0;

        @Test
        void recursive_with_clause_parser_once() {
            recursiveTimes = 1;
            TestProcedure testProcedure = givenProcedureWithCode("");
            TestNode node = new TestNode();
            NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                return of(node);
            };

            Clause<TestContext, TestNode> clause = mock(Clause.class);
            TestNode expression = new TestNode();
            when(clause.expression(node)).thenReturn(expression);
            ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                if (recursiveTimes-- > 0)
                    return of(clause);
                return empty();
            };

            assertThat(nodeParser.recursive(clauseParser).parse(testProcedure).get()).isSameAs(expression);
        }

        @Test
        void recursive_with_clause_parser_twice() {
            recursiveTimes = 2;
            TestProcedure testProcedure = givenProcedureWithCode("");
            TestNode node = new TestNode();
            NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                return of(node);
            };

            Clause<TestContext, TestNode> clause = mock(Clause.class);
            TestNode expression = new TestNode();
            when(clause.expression(node)).thenReturn(expression);

            Clause<TestContext, TestNode> clause2 = mock(Clause.class);
            TestNode expression2 = new TestNode();
            when(clause2.expression(expression)).thenReturn(expression2);

            ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                switch (recursiveTimes--) {
                    case 2:
                        return of(clause);
                    case 1:
                        return of(clause2);
                    default:
                        return empty();
                }
            };

            assertThat(nodeParser.recursive(clauseParser).parse(testProcedure).get()).isSameAs(expression2);
        }
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

        @Test
        void concat_with_clause_parser() {
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

            assertThat(nodeMandatory.concat(clauseParser).parse(testProcedure)).isSameAs(expression);
        }

        @Test
        void concat_with_empty_clause_parser() {
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

        @Nested
        class Recursive {

            @Test
            void recursive_with_empty_clause_parser() {
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

                assertThat(nodeMandatory.recursive(clauseParser).parse(testProcedure)).isSameAs(node);
            }

            int recursiveTimes = 0;

            @Test
            void recursive_with_clause_parser_once() {
                recursiveTimes = 1;
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
                    if (recursiveTimes-- > 0)
                        return of(clause);
                    return empty();
                };

                assertThat(nodeMandatory.recursive(clauseParser).parse(testProcedure)).isSameAs(expression);
            }

            @Test
            void recursive_with_clause_parser_twice() {
                recursiveTimes = 2;
                TestProcedure testProcedure = givenProcedureWithCode("");
                TestNode node = new TestNode();
                NodeParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeMandatory = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    return node;
                };

                Clause<TestContext, TestNode> clause = mock(Clause.class);
                TestNode expression = new TestNode();
                when(clause.expression(node)).thenReturn(expression);

                Clause<TestContext, TestNode> clause2 = mock(Clause.class);
                TestNode expression2 = new TestNode();
                when(clause2.expression(expression)).thenReturn(expression2);

                ClauseParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> clauseParser = procedure -> {
                    assertThat(procedure).isSameAs(testProcedure);
                    switch (recursiveTimes--) {
                        case 2:
                            return of(clause);
                        case 1:
                            return of(clause2);
                        default:
                            return empty();
                    }
                };

                assertThat(nodeMandatory.recursive(clauseParser).parse(testProcedure)).isSameAs(expression2);
            }
        }
    }

    @Test
    void clause_with_mandatory() {
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
