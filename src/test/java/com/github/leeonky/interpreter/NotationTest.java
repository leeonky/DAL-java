package com.github.leeonky.interpreter;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.Optional;

import static com.github.leeonky.interpreter.Notation.notation;
import static java.util.Collections.emptyMap;
import static org.assertj.core.api.Assertions.assertThat;

class NotationTest {

    @Nested
    class Node {
        NodeParser<TestContext, TestNode, ?, ?, TestProcedure> nodeParser = notation("true").node(TestNode::new);

        @Test
        void return_empty_when_not_match() {
            SourceCode sourceCode = new SourceCode("not match");

            assertThat(nodeParser.parse(new TestProcedure(sourceCode))).isEmpty();

            assertThat(sourceCode.popChar(emptyMap())).isEqualTo('n');
        }

        @Test
        void trim_left_blank_even_not_match() {
            SourceCode sourceCode = new SourceCode(" not match");

            assertThat(nodeParser.parse(new TestProcedure(sourceCode))).isEmpty();

            assertThat(sourceCode.popChar(emptyMap())).isEqualTo('n');
        }


        @Test
        void return_node_when_matches() {
            SourceCode sourceCode = new SourceCode("true");

            Optional<TestNode> testNode = nodeParser.parse(new TestProcedure(sourceCode));

            assertThat(testNode.get().getPositionBegin()).isEqualTo(0);
            assertThat(testNode.get().getContent()).isEqualTo("true");
            assertThat(sourceCode.hasCode()).isFalse();
        }

        @Test
        void trim_left_blank_before_matching() {
            SourceCode sourceCode = new SourceCode(" true");

            Optional<TestNode> testNode = nodeParser.parse(new TestProcedure(sourceCode));

            assertThat(testNode.get().getPositionBegin()).isEqualTo(1);
            assertThat(testNode.get().getContent()).isEqualTo("true");
            assertThat(sourceCode.hasCode()).isFalse();
        }
    }

    @Nested
    class Operator {

        @Test
        void return_when_match_symbol() {
            TestOperator testOperator = new TestOperator();
            SourceCode sourceCode = new SourceCode(" +=");
            OperatorParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> operatorParser =
                    notation("+=").operator(() -> testOperator);

            TestOperator testOperator2 = operatorParser.parse(new TestProcedure(sourceCode)).get();

            assertThat(testOperator2).isSameAs(testOperator);
            assertThat(testOperator2.getPosition()).isEqualTo(1);
        }

        @Test
        void return_empty_when_not_match() {
            SourceCode sourceCode = new SourceCode(" +=");
            OperatorParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> operatorParser =
                    notation("++").operator(TestOperator::new);

            assertThat(operatorParser.parse(new TestProcedure(sourceCode))).isEmpty();
            assertThat(sourceCode.popChar(emptyMap())).isEqualTo('+');
        }

        @Test
        void return_empty_when_predicate_false() {
            TestOperator testOperator = new TestOperator();
            SourceCode sourceCode = new SourceCode(" +=");
            TestProcedure testProcedure = new TestProcedure(sourceCode);

            OperatorParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> operatorParser =
                    notation("+=").operator(() -> testOperator, scanner1 -> {
                        assertThat(scanner1).isSameAs(testProcedure);
                        return false;
                    });

            assertThat(operatorParser.parse(testProcedure)).isEmpty();
            assertThat(sourceCode.popChar(emptyMap())).isEqualTo('+');
        }
    }

    @Nested
    class NotationWithMandatory {
        NodeParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> mandatory =
                notation("a").<TestContext, TestNode, TestExpression, TestOperator, TestProcedure>node(TestNode::new).mandatory("");

        @Test
        void return_empty_when_not_start_with() {
            SourceCode sourceCode = new SourceCode("not match");

            assertThat(notation("s").with(mandatory).parse(new TestProcedure(sourceCode))).isEmpty();
            assertThat(sourceCode.popChar(emptyMap())).isEqualTo('n');
        }

        @Test
        void left_trim_even_not_match() {
            SourceCode sourceCode = new SourceCode(" not match");

            assertThat(notation("s").with(mandatory).parse(new TestProcedure(sourceCode))).isEmpty();
            assertThat(sourceCode.popChar(emptyMap())).isEqualTo('n');
        }

        @Test
        void return_node_when_matches() {
            SourceCode sourceCode = new SourceCode("(a");

            Optional<TestNode> testNode = notation("(").with(mandatory).parse(new TestProcedure(sourceCode));

            assertThat(testNode.get().getContent()).isEqualTo("a");
            assertThat(testNode.get().getPositionBegin()).isEqualTo(0);
            assertThat(sourceCode.hasCode()).isFalse();
        }

        @Test
        void trim_and_return_node_when_matches() {
            SourceCode sourceCode = new SourceCode(" (a");

            Optional<TestNode> testNode = notation("(").with(mandatory).parse(new TestProcedure(sourceCode));

            assertThat(testNode.get().getContent()).isEqualTo("a");
            assertThat(testNode.get().getPositionBegin()).isEqualTo(1);
            assertThat(sourceCode.hasCode()).isFalse();
        }
    }

    @Nested
    class BeforeMandatory {
        NodeParser.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> mandatory =
                notation("a").<TestContext, TestNode, TestExpression, TestOperator, TestProcedure>node(TestNode::new).mandatory("");


        @Test
        void return_empty_when_not_start_with() {
            SourceCode sourceCode = new SourceCode("not match");

            assertThat(notation("s").before(mandatory).parse(new TestProcedure(sourceCode))).isEmpty();
            assertThat(sourceCode.popChar(emptyMap())).isEqualTo('n');
        }

        @Test
        void left_trim_even_not_match() {
            SourceCode sourceCode = new SourceCode(" not match");

            assertThat(notation("s").before(mandatory).parse(new TestProcedure(sourceCode))).isEmpty();
            assertThat(sourceCode.popChar(emptyMap())).isEqualTo('n');
        }

        @Test
        void return_node_when_matches() {
            SourceCode sourceCode = new SourceCode("(a");

            Optional<TestNode> testNode = notation("(").before(mandatory).parse(new TestProcedure(sourceCode));

            assertThat(testNode.get().getContent()).isEqualTo("a");
            assertThat(testNode.get().getPositionBegin()).isEqualTo(1);
            assertThat(sourceCode.hasCode()).isFalse();
        }

        @Test
        void trim_and_return_node_when_matches() {
            SourceCode sourceCode = new SourceCode(" (a");

            Optional<TestNode> testNode = notation("(").before(mandatory).parse(new TestProcedure(sourceCode));

            assertThat(testNode.get().getContent()).isEqualTo("a");
            assertThat(testNode.get().getPositionBegin()).isEqualTo(2);
            assertThat(sourceCode.hasCode()).isFalse();
        }
    }

    @Nested
    class BeforeParser {
        NodeParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> parser =
                notation("a").node(TestNode::new);


        @Test
        void return_empty_when_not_start_with() {
            SourceCode sourceCode = new SourceCode("not match");

            assertThat(notation("s").before(parser).parse(new TestProcedure(sourceCode))).isEmpty();
            assertThat(sourceCode.popChar(emptyMap())).isEqualTo('n');
        }

        @Test
        void left_trim_even_not_match() {
            SourceCode sourceCode = new SourceCode(" not match");

            assertThat(notation("s").before(parser).parse(new TestProcedure(sourceCode))).isEmpty();
            assertThat(sourceCode.popChar(emptyMap())).isEqualTo(' ');
        }

        @Test
        void return_node_when_matches() {
            SourceCode sourceCode = new SourceCode("(a");

            Optional<TestNode> testNode = notation("(").before(parser).parse(new TestProcedure(sourceCode));

            assertThat(testNode.get().getContent()).isEqualTo("a");
            assertThat(testNode.get().getPositionBegin()).isEqualTo(1);
            assertThat(sourceCode.hasCode()).isFalse();
        }

        @Test
        void trim_and_return_node_when_matches() {
            SourceCode sourceCode = new SourceCode(" (a");

            Optional<TestNode> testNode = notation("(").before(parser).parse(new TestProcedure(sourceCode));

            assertThat(testNode.get().getContent()).isEqualTo("a");
            assertThat(testNode.get().getPositionBegin()).isEqualTo(2);
            assertThat(sourceCode.hasCode()).isFalse();
        }
    }
}