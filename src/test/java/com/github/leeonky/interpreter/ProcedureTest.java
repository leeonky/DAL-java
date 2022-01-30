package com.github.leeonky.interpreter;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import static com.github.leeonky.interpreter.SourceCodeTest.NO_ESCAPE;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.fail;

class ProcedureTest {

    @Nested
    class NodeWithOneChild {

        @Test
        void return_empty_when_not_match_opening_char() {
            TestProcedure testProcedure = givenProcedureWithCode("[1]");

            assertThat(testProcedure.fetchNodeWithOneChildNodeBetween('(', n -> fail(), ')', n -> fail(), "")).isEmpty();
            assertThat(testProcedure.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('[');
        }

        @Test
        void throw_error_when_more_than_one_child() {
            TestProcedure testProcedure = givenProcedureWithCode("(1, 2)");

            assertThat(assertThrows(SyntaxException.class, () -> testProcedure.fetchNodeWithOneChildNodeBetween('(', procedure -> {
                        procedure.getSourceCode().popChar(NO_ESCAPE);
                        return new TestNode(null);
                    }, ')', n -> fail(),
                    "error"))).hasMessageContaining("error");
        }

        @Test
        void parse_node() {
            TestProcedure testProcedure = givenProcedureWithCode("(1)");
            TestNode child = new TestNode(null);
            TestNode parent = new TestNode(null);

            assertThat(testProcedure.fetchNodeWithOneChildNodeBetween('(', procedure -> {
                assertThat(procedure.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('1');
                return child;
            }, ')', n -> {
                assertThat(n).isSameAs(child);
                return parent;
            }, "")).hasValue(parent);

        }
    }

    @Nested
    class NodeWithManyChildren {

        @Test
        void return_empty_when_not_match_opening_char() {
            TestProcedure testProcedure = givenProcedureWithCode("[1,2]");

            assertThat(testProcedure.fetchNodeWithElementsBetween('(', ')', n -> fail(), () -> fail())).isEmpty();
            assertThat(testProcedure.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('[');
        }

        @Test
        void parse_node() {
            TestProcedure testProcedure = givenProcedureWithCode("(1, 2)");

            TestNode testNode = testProcedure.fetchNodeWithElementsBetween('(', ')', TestNode::new,
                    () -> testProcedure.getSourceCode().popChar(NO_ESCAPE)).get();

            assertThat((List<Character>) testNode.getContent()).containsExactly('1', '2');
        }
    }

    @Nested
    class FetchString {

        @Test
        void return_empty_when_not_match_opening_char() {
            TestProcedure testProcedure = givenProcedureWithCode("'a'");

            assertThat(testProcedure.fetchString('"', '"', s -> fail(), new HashMap<>())).isEmpty();
            assertThat(testProcedure.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('\'');
        }

        @Test
        void return_empty_string() {
            TestProcedure testProcedure = givenProcedureWithCode(" '\"");
            TestNode actual = testProcedure.fetchString('\'', '"', TestNode::new, new HashMap<>()).get();
            assertThat(actual.getContent()).isEqualTo("");
            assertThat(actual.getPositionBegin()).isEqualTo(1);
        }

        @Test
        void escape_char() {
            TestProcedure testProcedure = givenProcedureWithCode(" 'a\\x'");
            TestNode actual = testProcedure.fetchString('\'', '\'', TestNode::new, new HashMap<String, Character>() {{
                put("\\x", 'y');
            }}).get();
            assertThat(actual.getContent()).isEqualTo("ay");
            assertThat(actual.getPositionBegin()).isEqualTo(1);
        }

        @Test
        void raise_error_when_not_finish() {
            TestProcedure testProcedure = givenProcedureWithCode(" 'a");
            SyntaxException syntaxException = assertThrows(SyntaxException.class, () ->
                    testProcedure.fetchString('\'', '\'', TestNode::new, new HashMap<>()));
            assertThat(syntaxException).hasMessageContaining("should end with `'`");

            assertThat(syntaxException.show(" 'a")).isEqualTo(" 'a\n   ^");
        }
    }

    @Nested
    class FetchBetween {

        @Test
        void return_empty_when_not_match_opening_char() {
            TestProcedure testProcedure = givenProcedureWithCode("'a'");

            assertThat(testProcedure.fetchBetween("(", ")", () -> fail())).isEmpty();
            assertThat(testProcedure.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('\'');
        }

        @Test
        void return_object_between_opening_and_closing() {
            TestProcedure testProcedure = givenProcedureWithCode("(a)");

            assertThat(testProcedure.fetchBetween("(", ")", () ->
                    testProcedure.getSourceCode().popChar(NO_ESCAPE))).hasValue('a');
            assertThat(testProcedure.getSourceCode().isEndOfLine()).isTrue();
        }

        @Test
        void raise_error_when_no_closing() {
            TestProcedure testProcedure = givenProcedureWithCode("(a");

            assertThat(assertThrows(SyntaxException.class, () -> testProcedure.fetchBetween("(", ")", () ->
                    testProcedure.getSourceCode().popChar(NO_ESCAPE)))).hasMessageContaining("should end with `)`");
        }
    }

    @Nested
    class FetchNodeBetween {

        public Optional<TestNode> oneCharNode(TestProcedure testProcedure) {
            return Optional.of(new TestNode(testProcedure.getSourceCode().popChar(NO_ESCAPE)));
        }

        @Test
        void return_empty_when_not_match_opening_char() {
            TestProcedure testProcedure = givenProcedureWithCode("'a'");

            assertThat(testProcedure.fetchNodeBetween("(", ")", this::oneCharNode)).isEmpty();
            assertThat(testProcedure.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('\'');
        }

        @Test
        void return_object_between_opening_and_closing() {
            TestProcedure testProcedure = givenProcedureWithCode("(a)");

            TestNode testNode = testProcedure.fetchNodeBetween("(", ")", this::oneCharNode).get();
            assertThat(testNode.getContent()).isEqualTo('a');
            assertThat(testProcedure.getSourceCode().isEndOfLine()).isTrue();
        }

        @Test
        void raise_error_when_no_closing() {
            TestProcedure testProcedure = givenProcedureWithCode("(a");

            assertThat(assertThrows(SyntaxException.class, () -> testProcedure.fetchNodeBetween("(", ")", this::oneCharNode)))
                    .hasMessageContaining("should end with `)`");
        }

        @Test
        void return_empty_when_fetch_empty_between_opening_and_closing() {
            TestProcedure procedure = givenProcedureWithCode("'a'");

            assertThat(procedure.fetchNodeBetween("'", "'", testProcedure -> Optional.empty())).isEmpty();
            assertThat(procedure.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('\'');
        }
    }

    @Nested
    class FetchBySplit {

        @Test
        void return_one_node_list() {
            TestProcedure testProcedure = givenProcedureWithCode("a");
            assertThat(testProcedure.fetchNodesSplitBy(",", procedure -> new TestNode(testProcedure.getSourceCode().popChar(NO_ESCAPE))).stream()
                    .map(TestNode::getContent).collect(Collectors.toList())).containsExactly('a');
        }

        @Test
        void return_node_list() {
            TestProcedure testProcedure = givenProcedureWithCode("a,b,c");
            assertThat(testProcedure.fetchNodesSplitBy(",", procedure -> new TestNode(testProcedure.getSourceCode().popChar(NO_ESCAPE))).stream()
                    .map(TestNode::getContent).collect(Collectors.toList())).containsExactly('a', 'b', 'c');
        }
    }

    private TestProcedure givenProcedureWithCode(String s) {
        return new TestProcedure(new SourceCode(s));
    }
}