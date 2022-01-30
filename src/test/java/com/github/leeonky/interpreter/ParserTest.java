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

class ParserTest {

    @Nested
    class NodeWithOneChild {

        @Test
        void return_empty_when_not_match_opening_char() {
            TestParser testParser = givenParserWithCode("[1]");

            assertThat(testParser.fetchNodeWithOneChildNodeBetween('(', ')', n -> fail(), n -> fail(), "")).isEmpty();
            assertThat(testParser.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('[');
        }

        @Test
        void throw_error_when_more_than_one_child() {
            TestParser testParser = givenParserWithCode("(1, 2)");

            assertThat(assertThrows(SyntaxException.class, () -> testParser.fetchNodeWithOneChildNodeBetween('(', ')', n -> fail(),
                    parser -> {
                        parser.getSourceCode().popChar(NO_ESCAPE);
                        return new TestNode(null);
                    }, "error"))).hasMessageContaining("error");
        }

        @Test
        void parse_node() {
            TestParser testParser = givenParserWithCode("(1)");
            TestNode child = new TestNode(null);
            TestNode parent = new TestNode(null);

            assertThat(testParser.fetchNodeWithOneChildNodeBetween('(', ')', n -> {
                assertThat(n).isSameAs(child);
                return parent;
            }, parser -> {
                assertThat(parser.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('1');
                return child;
            }, "")).hasValue(parent);

        }
    }

    @Nested
    class NodeWithManyChildren {

        @Test
        void return_empty_when_not_match_opening_char() {
            TestParser testParser = givenParserWithCode("[1,2]");

            assertThat(testParser.fetchNodeWithElementsBetween('(', ')', n -> fail(), () -> fail())).isEmpty();
            assertThat(testParser.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('[');
        }

        @Test
        void parse_node() {
            TestParser testParser = givenParserWithCode("(1, 2)");

            TestNode testNode = testParser.fetchNodeWithElementsBetween('(', ')', TestNode::new,
                    () -> testParser.getSourceCode().popChar(NO_ESCAPE)).get();

            assertThat((List<Character>) testNode.getContent()).containsExactly('1', '2');
        }
    }

    @Nested
    class FetchString {

        @Test
        void return_empty_when_not_match_opening_char() {
            TestParser testParser = givenParserWithCode("'a'");

            assertThat(testParser.fetchString('"', '"', s -> fail(), new HashMap<>())).isEmpty();
            assertThat(testParser.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('\'');
        }

        @Test
        void return_empty_string() {
            TestParser testParser = givenParserWithCode(" '\"");
            TestNode actual = testParser.fetchString('\'', '"', TestNode::new, new HashMap<>()).get();
            assertThat(actual.getContent()).isEqualTo("");
            assertThat(actual.getPositionBegin()).isEqualTo(1);
        }

        @Test
        void escape_char() {
            TestParser testParser = givenParserWithCode(" 'a\\x'");
            TestNode actual = testParser.fetchString('\'', '\'', TestNode::new, new HashMap<String, Character>() {{
                put("\\x", 'y');
            }}).get();
            assertThat(actual.getContent()).isEqualTo("ay");
            assertThat(actual.getPositionBegin()).isEqualTo(1);
        }

        @Test
        void raise_error_when_not_finish() {
            TestParser testParser = givenParserWithCode(" 'a");
            SyntaxException syntaxException = assertThrows(SyntaxException.class, () ->
                    testParser.fetchString('\'', '\'', TestNode::new, new HashMap<>()));
            assertThat(syntaxException).hasMessageContaining("should end with `'`");

            assertThat(syntaxException.show(" 'a")).isEqualTo(" 'a\n   ^");
        }
    }

    @Nested
    class FetchBetween {

        @Test
        void return_empty_when_not_match_opening_char() {
            TestParser testParser = givenParserWithCode("'a'");

            assertThat(testParser.fetchBetween("(", ")", () -> fail())).isEmpty();
            assertThat(testParser.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('\'');
        }

        @Test
        void return_object_between_opening_and_closing() {
            TestParser testParser = givenParserWithCode("(a)");

            assertThat(testParser.fetchBetween("(", ")", () ->
                    testParser.getSourceCode().popChar(NO_ESCAPE))).hasValue('a');
            assertThat(testParser.getSourceCode().isEndOfLine()).isTrue();
        }

        @Test
        void raise_error_when_no_closing() {
            TestParser testParser = givenParserWithCode("(a");

            assertThat(assertThrows(SyntaxException.class, () -> testParser.fetchBetween("(", ")", () ->
                    testParser.getSourceCode().popChar(NO_ESCAPE)))).hasMessageContaining("should end with `)`");
        }
    }

    @Nested
    class FetchNodeBetween {

        public Optional<TestNode> oneCharNode(TestParser testParser) {
            return Optional.of(new TestNode(testParser.getSourceCode().popChar(NO_ESCAPE)));
        }

        @Test
        void return_empty_when_not_match_opening_char() {
            TestParser testParser = givenParserWithCode("'a'");

            assertThat(testParser.fetchNodeBetween("(", ")", this::oneCharNode)).isEmpty();
            assertThat(testParser.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('\'');
        }

        @Test
        void return_object_between_opening_and_closing() {
            TestParser testParser = givenParserWithCode("(a)");

            TestNode testNode = testParser.fetchNodeBetween("(", ")", this::oneCharNode).get();
            assertThat(testNode.getContent()).isEqualTo('a');
            assertThat(testParser.getSourceCode().isEndOfLine()).isTrue();
        }

        @Test
        void raise_error_when_no_closing() {
            TestParser testParser = givenParserWithCode("(a");

            assertThat(assertThrows(SyntaxException.class, () -> testParser.fetchNodeBetween("(", ")", this::oneCharNode)))
                    .hasMessageContaining("should end with `)`");
        }

        @Test
        void return_empty_when_fetch_empty_between_opening_and_closing() {
            TestParser parser = givenParserWithCode("'a'");

            assertThat(parser.fetchNodeBetween("'", "'", testParser -> Optional.empty())).isEmpty();
            assertThat(parser.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('\'');
        }
    }

    @Nested
    class FetchBySplit {

        @Test
        void return_one_node_list() {
            TestParser testParser = givenParserWithCode("a");
            assertThat(testParser.fetchNodesSplitBy(",", parser -> new TestNode(testParser.getSourceCode().popChar(NO_ESCAPE))).stream()
                    .map(TestNode::getContent).collect(Collectors.toList())).containsExactly('a');
        }

        @Test
        void return_node_list() {
            TestParser testParser = givenParserWithCode("a,b,c");
            assertThat(testParser.fetchNodesSplitBy(",", parser -> new TestNode(testParser.getSourceCode().popChar(NO_ESCAPE))).stream()
                    .map(TestNode::getContent).collect(Collectors.toList())).containsExactly('a', 'b', 'c');
        }
    }

    private TestParser givenParserWithCode(String s) {
        SourceCode sourceCode = new SourceCode(s);
        return new TestParser(sourceCode);
    }
}