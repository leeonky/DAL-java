package com.github.leeonky.interpreter;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.List;
import java.util.Optional;

import static com.github.leeonky.interpreter.SourceCodeTest.NO_ESCAPE;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.fail;

class TokenParserTest {

    @Nested
    class NodeWithOneChild {

        @Test
        void return_empty_when_not_match_opening_char() {
            TestTokenParser tokenParser = givenTokenParserWithCode("[1]");

            assertThat(tokenParser.fetchNodeWithOneChildNodeBetween('(', ')', n -> fail(), n -> fail(), "")).isEmpty();
            assertThat(tokenParser.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('[');
        }

        @Test
        void throw_error_when_more_than_one_child() {
            TestTokenParser tokenParser = givenTokenParserWithCode("(1, 2)");

            assertThat(assertThrows(SyntaxException.class, () -> tokenParser.fetchNodeWithOneChildNodeBetween('(', ')', n -> fail(),
                    parser -> {
                        parser.getSourceCode().popChar(NO_ESCAPE);
                        return new TestNode(null);
                    }, "error"))).hasMessageContaining("error");
        }

        @Test
        void parse_node() {
            TestTokenParser tokenParser = givenTokenParserWithCode("(1)");
            TestNode child = new TestNode(null);
            TestNode parent = new TestNode(null);

            assertThat(tokenParser.fetchNodeWithOneChildNodeBetween('(', ')', n -> {
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
            TestTokenParser tokenParser = givenTokenParserWithCode("[1,2]");

            assertThat(tokenParser.fetchNodeWithElementsBetween('(', ')', n -> fail(), () -> fail())).isEmpty();
            assertThat(tokenParser.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('[');
        }

        @Test
        void parse_node() {
            TestTokenParser tokenParser = givenTokenParserWithCode("(1, 2)");

            TestNode testNode = tokenParser.fetchNodeWithElementsBetween('(', ')', TestNode::new,
                    () -> tokenParser.getSourceCode().popChar(NO_ESCAPE)).get();

            assertThat((List<Character>) testNode.getContent()).containsExactly('1', '2');
        }
    }

    @Nested
    class FetchString {

        @Test
        void return_empty_when_not_match_opening_char() {
            TestTokenParser tokenParser = givenTokenParserWithCode("'a'");

            assertThat(tokenParser.fetchString('"', '"', s -> fail(), new HashMap<>())).isEmpty();
            assertThat(tokenParser.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('\'');
        }

        @Test
        void return_empty_string() {
            TestTokenParser tokenParser = givenTokenParserWithCode(" '\"");
            TestNode actual = tokenParser.fetchString('\'', '"', TestNode::new, new HashMap<>()).get();
            assertThat(actual.getContent()).isEqualTo("");
            assertThat(actual.getPositionBegin()).isEqualTo(1);
        }

        @Test
        void escape_char() {
            TestTokenParser tokenParser = givenTokenParserWithCode(" 'a\\x'");
            TestNode actual = tokenParser.fetchString('\'', '\'', TestNode::new, new HashMap<String, Character>() {{
                put("\\x", 'y');
            }}).get();
            assertThat(actual.getContent()).isEqualTo("ay");
            assertThat(actual.getPositionBegin()).isEqualTo(1);
        }

        @Test
        void raise_error_when_not_finish() {
            TestTokenParser tokenParser = givenTokenParserWithCode(" 'a");
            SyntaxException syntaxException = assertThrows(SyntaxException.class, () ->
                    tokenParser.fetchString('\'', '\'', TestNode::new, new HashMap<>()));
            assertThat(syntaxException).hasMessageContaining("should end with `'`");

            assertThat(syntaxException.show(" 'a")).isEqualTo(" 'a\n   ^");
        }
    }

    @Nested
    class FetchBetween {

        @Test
        void return_empty_when_not_match_opening_char() {
            TestTokenParser tokenParser = givenTokenParserWithCode("'a'");

            assertThat(tokenParser.fetchBetween("(", ")", () -> fail())).isEmpty();
            assertThat(tokenParser.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('\'');
        }

        @Test
        void return_object_between_opening_and_closing() {
            TestTokenParser tokenParser = givenTokenParserWithCode("(a)");

            assertThat(tokenParser.fetchBetween("(", ")", () ->
                    tokenParser.getSourceCode().popChar(NO_ESCAPE))).hasValue('a');
            assertThat(tokenParser.getSourceCode().isEndOfLine()).isTrue();
        }

        @Test
        void raise_error_when_no_closing() {
            TestTokenParser tokenParser = givenTokenParserWithCode("(a");

            assertThat(assertThrows(SyntaxException.class, () -> tokenParser.fetchBetween("(", ")", () ->
                    tokenParser.getSourceCode().popChar(NO_ESCAPE)))).hasMessageContaining("should end with `)`");
        }
    }

    @Nested
    class FetchNodeBetween {

        public Optional<TestNode> oneCharNode(TestTokenParser parser) {
            return Optional.of(new TestNode(parser.getSourceCode().popChar(NO_ESCAPE)));
        }

        @Test
        void return_empty_when_not_match_opening_char() {
            TestTokenParser tokenParser = givenTokenParserWithCode("'a'");

            assertThat(tokenParser.fetchNodeBetween("(", ")", this::oneCharNode)).isEmpty();
            assertThat(tokenParser.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('\'');
        }

        @Test
        void return_object_between_opening_and_closing() {
            TestTokenParser tokenParser = givenTokenParserWithCode("(a)");

            TestNode testNode = tokenParser.fetchNodeBetween("(", ")", this::oneCharNode).get();
            assertThat(testNode.getContent()).isEqualTo('a');
            assertThat(tokenParser.getSourceCode().isEndOfLine()).isTrue();
        }

        @Test
        void raise_error_when_no_closing() {
            TestTokenParser tokenParser = givenTokenParserWithCode("(a");

            assertThat(assertThrows(SyntaxException.class, () -> tokenParser.fetchNodeBetween("(", ")", this::oneCharNode)))
                    .hasMessageContaining("should end with `)`");
        }

        @Test
        void return_empty_when_fetch_empty_between_opening_and_closing() {
            TestTokenParser tokenParser = givenTokenParserWithCode("'a'");

            assertThat(tokenParser.fetchNodeBetween("'", "'", parser -> Optional.empty())).isEmpty();
            assertThat(tokenParser.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('\'');
        }
    }

    @Nested
    class FetchBySplit {

        @Test
        void return_one_node_list() {

        }
    }

    private TestTokenParser givenTokenParserWithCode(String s) {
        SourceCode sourceCode = new SourceCode(s);
        return new TestTokenParser(sourceCode);
    }
}