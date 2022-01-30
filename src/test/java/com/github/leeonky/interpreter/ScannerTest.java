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

class ScannerTest {

    @Nested
    class NodeWithOneChild {

        @Test
        void return_empty_when_not_match_opening_char() {
            TestScanner scanner = givenScannerWithCode("[1]");

            assertThat(scanner.fetchNodeWithOneChildNodeBetween('(', ')', n -> fail(), n -> fail(), "")).isEmpty();
            assertThat(scanner.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('[');
        }

        @Test
        void throw_error_when_more_than_one_child() {
            TestScanner testScanner = givenScannerWithCode("(1, 2)");

            assertThat(assertThrows(SyntaxException.class, () -> testScanner.fetchNodeWithOneChildNodeBetween('(', ')', n -> fail(),
                    scanner -> {
                        scanner.getSourceCode().popChar(NO_ESCAPE);
                        return new TestNode(null);
                    }, "error"))).hasMessageContaining("error");
        }

        @Test
        void parse_node() {
            TestScanner testScanner = givenScannerWithCode("(1)");
            TestNode child = new TestNode(null);
            TestNode parent = new TestNode(null);

            assertThat(testScanner.fetchNodeWithOneChildNodeBetween('(', ')', n -> {
                assertThat(n).isSameAs(child);
                return parent;
            }, scanner -> {
                assertThat(scanner.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('1');
                return child;
            }, "")).hasValue(parent);

        }
    }

    @Nested
    class NodeWithManyChildren {

        @Test
        void return_empty_when_not_match_opening_char() {
            TestScanner testScanner = givenScannerWithCode("[1,2]");

            assertThat(testScanner.fetchNodeWithElementsBetween('(', ')', n -> fail(), () -> fail())).isEmpty();
            assertThat(testScanner.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('[');
        }

        @Test
        void parse_node() {
            TestScanner testScanner = givenScannerWithCode("(1, 2)");

            TestNode testNode = testScanner.fetchNodeWithElementsBetween('(', ')', TestNode::new,
                    () -> testScanner.getSourceCode().popChar(NO_ESCAPE)).get();

            assertThat((List<Character>) testNode.getContent()).containsExactly('1', '2');
        }
    }

    @Nested
    class FetchString {

        @Test
        void return_empty_when_not_match_opening_char() {
            TestScanner scanner = givenScannerWithCode("'a'");

            assertThat(scanner.fetchString('"', '"', s -> fail(), new HashMap<>())).isEmpty();
            assertThat(scanner.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('\'');
        }

        @Test
        void return_empty_string() {
            TestScanner scanner = givenScannerWithCode(" '\"");
            TestNode actual = scanner.fetchString('\'', '"', TestNode::new, new HashMap<>()).get();
            assertThat(actual.getContent()).isEqualTo("");
            assertThat(actual.getPositionBegin()).isEqualTo(1);
        }

        @Test
        void escape_char() {
            TestScanner scanner = givenScannerWithCode(" 'a\\x'");
            TestNode actual = scanner.fetchString('\'', '\'', TestNode::new, new HashMap<String, Character>() {{
                put("\\x", 'y');
            }}).get();
            assertThat(actual.getContent()).isEqualTo("ay");
            assertThat(actual.getPositionBegin()).isEqualTo(1);
        }

        @Test
        void raise_error_when_not_finish() {
            TestScanner scanner = givenScannerWithCode(" 'a");
            SyntaxException syntaxException = assertThrows(SyntaxException.class, () ->
                    scanner.fetchString('\'', '\'', TestNode::new, new HashMap<>()));
            assertThat(syntaxException).hasMessageContaining("should end with `'`");

            assertThat(syntaxException.show(" 'a")).isEqualTo(" 'a\n   ^");
        }
    }

    @Nested
    class FetchBetween {

        @Test
        void return_empty_when_not_match_opening_char() {
            TestScanner scanner = givenScannerWithCode("'a'");

            assertThat(scanner.fetchBetween("(", ")", () -> fail())).isEmpty();
            assertThat(scanner.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('\'');
        }

        @Test
        void return_object_between_opening_and_closing() {
            TestScanner scanner = givenScannerWithCode("(a)");

            assertThat(scanner.fetchBetween("(", ")", () ->
                    scanner.getSourceCode().popChar(NO_ESCAPE))).hasValue('a');
            assertThat(scanner.getSourceCode().isEndOfLine()).isTrue();
        }

        @Test
        void raise_error_when_no_closing() {
            TestScanner scanner = givenScannerWithCode("(a");

            assertThat(assertThrows(SyntaxException.class, () -> scanner.fetchBetween("(", ")", () ->
                    scanner.getSourceCode().popChar(NO_ESCAPE)))).hasMessageContaining("should end with `)`");
        }
    }

    @Nested
    class FetchNodeBetween {

        public Optional<TestNode> oneCharNode(TestScanner scanner) {
            return Optional.of(new TestNode(scanner.getSourceCode().popChar(NO_ESCAPE)));
        }

        @Test
        void return_empty_when_not_match_opening_char() {
            TestScanner testScanner = givenScannerWithCode("'a'");

            assertThat(testScanner.fetchNodeBetween("(", ")", this::oneCharNode)).isEmpty();
            assertThat(testScanner.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('\'');
        }

        @Test
        void return_object_between_opening_and_closing() {
            TestScanner testScanner = givenScannerWithCode("(a)");

            TestNode testNode = testScanner.fetchNodeBetween("(", ")", this::oneCharNode).get();
            assertThat(testNode.getContent()).isEqualTo('a');
            assertThat(testScanner.getSourceCode().isEndOfLine()).isTrue();
        }

        @Test
        void raise_error_when_no_closing() {
            TestScanner testScanner = givenScannerWithCode("(a");

            assertThat(assertThrows(SyntaxException.class, () -> testScanner.fetchNodeBetween("(", ")", this::oneCharNode)))
                    .hasMessageContaining("should end with `)`");
        }

        @Test
        void return_empty_when_fetch_empty_between_opening_and_closing() {
            TestScanner testScanner = givenScannerWithCode("'a'");

            assertThat(testScanner.fetchNodeBetween("'", "'", scanner -> Optional.empty())).isEmpty();
            assertThat(testScanner.getSourceCode().popChar(NO_ESCAPE)).isEqualTo('\'');
        }
    }

    @Nested
    class FetchBySplit {

        @Test
        void return_one_node_list() {
            TestScanner testScanner = givenScannerWithCode("a");
            assertThat(testScanner.fetchNodesSplitBy(",", scanner -> new TestNode(testScanner.getSourceCode().popChar(NO_ESCAPE))).stream()
                    .map(TestNode::getContent).collect(Collectors.toList())).containsExactly('a');
        }

        @Test
        void return_node_list() {
            TestScanner testScanner = givenScannerWithCode("a,b,c");
            assertThat(testScanner.fetchNodesSplitBy(",", scanner -> new TestNode(testScanner.getSourceCode().popChar(NO_ESCAPE))).stream()
                    .map(TestNode::getContent).collect(Collectors.toList())).containsExactly('a', 'b', 'c');
        }
    }

    private TestScanner givenScannerWithCode(String s) {
        SourceCode sourceCode = new SourceCode(s);
        return new TestScanner(sourceCode);
    }
}