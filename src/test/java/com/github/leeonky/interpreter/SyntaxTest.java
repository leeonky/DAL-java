package com.github.leeonky.interpreter;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.List;

import static com.github.leeonky.interpreter.Syntax.many;
import static com.github.leeonky.interpreter.Syntax.single;
import static java.util.Optional.*;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.*;

class SyntaxTest extends BaseTest {

    @Nested
    class SingleOP {

        @Test
        void return_empty_when_op_is_empty() {
            TestProcedure testProcedure = givenProcedureWithCode("");
            NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                return empty();
            };

            Syntax<TestContext, TestNode, TestExpression, TestOperator, TestProcedure, NodeParser<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, NodeParser.Mandatory<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, TestNode, NodeParser<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, TestNode> single = spy(single(nodeParser));

            assertThat(single.as().parse(testProcedure)).isEmpty();
            verify(single, never()).isClose(any());
            verify(single, never()).close(any());
        }

        @Test
        void return_single() {
            TestProcedure testProcedure = givenProcedureWithCode("");
            TestNode node = new TestNode();
            NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                return of(node);
            };

            Syntax<TestContext, TestNode, TestExpression, TestOperator, TestProcedure, NodeParser<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, NodeParser.Mandatory<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, TestNode, NodeParser<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, TestNode> single = spy(single(nodeParser));

            assertThat(single.as().parse(testProcedure).get()).isSameAs(node);

            verify(single).isClose(testProcedure);
            verify(single).close(testProcedure);
        }
    }

    @Nested
    class SingleMA {

        @Test
        void return_single() {
            TestProcedure testProcedure = givenProcedureWithCode("");
            TestNode node = new TestNode();
            NodeParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                return node;
            };

            Syntax<TestContext, TestNode, TestExpression, TestOperator, TestProcedure, NodeParser<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, NodeParser.Mandatory<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, TestNode, NodeParser.Mandatory<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, TestNode> single = spy(single(nodeParser));

            assertThat(single.as().parse(testProcedure)).isSameAs(node);

            verify(single).isClose(testProcedure);
            verify(single).close(testProcedure);
        }
    }

    @Nested
    class ManyOP {

        @Test
        void return_empty_when_op_is_empty() {
            TestProcedure testProcedure = givenProcedureWithCode("");
            NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                return empty();
            };

            TestNode node = new TestNode();

            Syntax<TestContext, TestNode, TestExpression, TestOperator, TestProcedure, NodeParser<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, NodeParser.Mandatory<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, TestNode, NodeParser.Mandatory<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, List<TestNode>> many = spy(many(nodeParser));

            assertThat(many.as(list -> {
                assertThat(list).isEmpty();
                return node;
            }).parse(testProcedure)).isSameAs(node);
            verify(many).close(testProcedure);
        }

        @Test
        void return_empty_when_start_with_close() {
            TestProcedure testProcedure = givenProcedureWithCode("");
            NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                return of(new TestNode());
            };

            TestNode node = new TestNode();

            Syntax<TestContext, TestNode, TestExpression, TestOperator, TestProcedure, NodeParser<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, NodeParser.Mandatory<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, TestNode, NodeParser.Mandatory<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, List<TestNode>> many = spy(many(nodeParser));

            when(many.isClose(testProcedure)).thenReturn(true);

            assertThat(many.as(list -> {
                assertThat(list).isEmpty();
                return node;
            }).parse(testProcedure)).isSameAs(node);
        }

        @Test
        void return_one_node_when_no_split() {
            TestNode node = new TestNode();
            TestProcedure testProcedure = givenProcedureWithCode("");
            NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                return of(node);
            };

            Syntax<TestContext, TestNode, TestExpression, TestOperator, TestProcedure, NodeParser<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, NodeParser.Mandatory<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, TestNode, NodeParser.Mandatory<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, List<TestNode>> many = spy(many(nodeParser));

            when(many.isSplitter(testProcedure)).thenReturn(false);

            TestNode result = new TestNode();

            assertThat(many.as(list -> {
                assertThat(list).containsExactly(node);
                return result;
            }).parse(testProcedure)).isSameAs(result);
        }

        int nodeCode;

        @Test
        void return_with_list_with_procedure_index() {
            nodeCode = 2;

            TestProcedure testProcedure = givenProcedureWithCode("");

            NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> nodeParser = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                if (nodeCode-- > 0)
                    return ofNullable(new TestNode(procedure.getIndex()));
                else
                    return empty();
            };

            Syntax<TestContext, TestNode, TestExpression, TestOperator, TestProcedure, NodeParser<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, NodeParser.Mandatory<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, TestNode, NodeParser.Mandatory<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, List<TestNode>> many = spy(many(nodeParser));

            TestNode node = new TestNode();
            assertThat(many.as(list -> {
                assertThat(list).hasSize(2).extracting("content").containsExactly(0, 1);
                return node;
            }).parse(testProcedure)).isSameAs(node);

            verify(many, times(3)).isClose(testProcedure);
            verify(many).close(testProcedure);
        }
    }

    @Nested
    class ManyMA {

        @Test
        void return_empty_when_start_with_close() {
            TestProcedure testProcedure = givenProcedureWithCode("");
            NodeParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> mandatory = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                return new TestNode();
            };

            TestNode node = new TestNode();

            Syntax<TestContext, TestNode, TestExpression, TestOperator, TestProcedure, NodeParser<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, NodeParser.Mandatory<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, TestNode, NodeParser.Mandatory<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, List<TestNode>> many = spy(many(mandatory));

            when(many.isClose(testProcedure)).thenReturn(true);

            assertThat(many.as(list -> {
                assertThat(list).isEmpty();
                return node;
            }).parse(testProcedure)).isSameAs(node);
        }

        @Test
        void return_one_node_when_no_split() {
            TestNode node = new TestNode();
            TestProcedure testProcedure = givenProcedureWithCode("");
            NodeParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> mandatory = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                return node;
            };

            Syntax<TestContext, TestNode, TestExpression, TestOperator, TestProcedure, NodeParser<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, NodeParser.Mandatory<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, TestNode, NodeParser.Mandatory<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, List<TestNode>> many = spy(many(mandatory));

            when(many.isSplitter(testProcedure)).thenReturn(false);

            TestNode result = new TestNode();

            assertThat(many.as(list -> {
                assertThat(list).containsExactly(node);
                return result;
            }).parse(testProcedure)).isSameAs(result);
        }

        int nodeCode;

        @Test
        void return_with_list_with_procedure_index() {
            nodeCode = 2;

            TestProcedure testProcedure = givenProcedureWithCode("");

            NodeParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> mandatory = procedure -> {
                assertThat(procedure).isSameAs(testProcedure);
                if (nodeCode-- > 0)
                    return new TestNode(procedure.getIndex());
                return null;
            };

            Syntax<TestContext, TestNode, TestExpression, TestOperator, TestProcedure, NodeParser<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, NodeParser.Mandatory<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, TestNode, NodeParser.Mandatory<TestContext, TestNode,
                    TestExpression, TestOperator, TestProcedure>, List<TestNode>> many = spy(many(mandatory));

            when(many.isSplitter(testProcedure)).thenAnswer(a -> nodeCode > 0);

            TestNode node = new TestNode();
            assertThat(many.as(list -> {
                assertThat(list).hasSize(2).extracting("content").containsExactly(0, 1);
                return node;
            }).parse(testProcedure)).isSameAs(node);

            verify(many, times(2)).isClose(testProcedure);
            verify(many).close(testProcedure);
        }
    }
}