package com.github.leeonky.interpreter;

import com.github.leeonky.dal.compiler.EscapeChars;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Optional;

import static com.github.leeonky.interpreter.SourceCode.FetchBy.BY_CHAR;
import static com.github.leeonky.interpreter.SourceCode.FetchBy.BY_NODE;
import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class SourceCodeTest {

    public static final HashMap<String, Character> NO_ESCAPE = new HashMap<>();

    @Nested
    class HasCode {

        @Test
        void should_has_code_when_position_not_end() {
            assertThat(new SourceCode("a").hasCode()).isTrue();
            assertThat(new SourceCode(" ").hasCode()).isTrue();
        }

        @Test
        void should_not_has_code_when_at_the_end() {
            assertThat(new SourceCode("").hasCode()).isFalse();
        }

        @Test
        void check_has_code_after_pop() {
            SourceCode code = new SourceCode("ab ");
            code.escapedPop(NO_ESCAPE);
            assertThat(code.hasCode()).isTrue();
            code.escapedPop(NO_ESCAPE);
            assertThat(code.hasCode()).isTrue();
            code.escapedPop(NO_ESCAPE);
            assertThat(code.hasCode()).isFalse();
        }
    }

    @Nested
    class StartsWith {

        @Test
        void start_with_given_word() {
            assertThat(new SourceCode("ab").startsWith("a")).isTrue();
        }

        @Test
        void not_start_with_given_word() {
            assertThat(new SourceCode("ab").startsWith("b")).isFalse();
        }

        @Test
        void trim_blank_and_start_with_given_word() {
            assertThat(new SourceCode(" \n\r\tab").startsWith("a")).isTrue();
        }

        @Test
        void starts_with_after_pop() {
            SourceCode sourceCode = new SourceCode("x \n\r\tab");
            sourceCode.escapedPop(NO_ESCAPE);
            assertThat(sourceCode.startsWith("a")).isTrue();
        }
    }

    @Nested
    class EscapedPop {

        @Test
        void pop_up_when_no_escape() {
            SourceCode sourceCode = new SourceCode("a");
            assertThat(sourceCode.escapedPop(NO_ESCAPE)).isEqualTo('a');
            assertThat(sourceCode.hasCode()).isFalse();
        }

        @Test
        void pop_up_when_escape_not_match() {
            SourceCode sourceCode = new SourceCode("a");
            assertThat(sourceCode.escapedPop(new EscapeChars().escape("b", 'a'))).isEqualTo('a');
            assertThat(sourceCode.hasCode()).isFalse();
        }

        @Test
        void pop_up_when_escaped() {
            SourceCode sourceCode = new SourceCode("aabbx");
            EscapeChars escape = new EscapeChars().escape("bb", 'a').escape("aa", 'b');
            assertThat(sourceCode.escapedPop(escape)).isEqualTo('b');
            assertThat(sourceCode.escapedPop(escape)).isEqualTo('a');
            assertThat(sourceCode.escapedPop(escape)).isEqualTo('x');
        }
    }

    @Nested
    class isBeginning {

        @Test
        void position_is_0() {
            assertThat(new SourceCode("a").isBeginning()).isTrue();
            assertThat(new SourceCode(" ").isBeginning()).isTrue();
            assertThat(new SourceCode("").isBeginning()).isTrue();
        }

        @Test
        void also_is_beginning_even_pop_blanks() {
            SourceCode sourceCode = new SourceCode(" \n\r\t");
            sourceCode.escapedPop(NO_ESCAPE);
            sourceCode.escapedPop(NO_ESCAPE);
            sourceCode.escapedPop(NO_ESCAPE);
            sourceCode.escapedPop(NO_ESCAPE);
            assertThat(sourceCode.isBeginning()).isTrue();
        }
    }

    @Nested
    class ThrowSyntaxException {

        @Test
        void throw_exception_with_position_info() {
            SourceCode sourceCode = new SourceCode("abc");
            sourceCode.escapedPop(NO_ESCAPE);
            assertThat(sourceCode.syntaxError("test", 1).show("abc")).isEqualTo("abc\n  ^");
        }
    }

    @Nested
    class PopWord {

        @Test
        void pop_matched_word() {
            SourceCode code = new SourceCode("ab");
            code.escapedPop(NO_ESCAPE);
            Token token = code.popWord("b").get();
            assertThat(token.getContent()).isEqualTo("b");
            assertThat(token.getPosition()).isEqualTo(1);
        }

        @Test
        void pop_not_matched_word() {
            SourceCode code = new SourceCode("ab");
            assertThat(code.popWord("b")).isEmpty();
            assertThat(code.escapedPop(NO_ESCAPE)).isEqualTo('a');
        }

        @Test
        void trim_before_pop_word() {
            SourceCode code = new SourceCode(" \n\t\rb");

            assertThat(code.popWord("b").get().getContent()).isEqualTo("b");
        }

        @Nested
        class ConditionalPop {

            @Test
            void non_matched_word() {
                SourceCode code = new SourceCode("ab");
                assertThat(code.popWord("a", () -> false)).isEmpty();
            }

            @Test
            void predicate_should_not_called_when_word_not_matched() {
                SourceCode code = new SourceCode("ab");
                assertThat(code.popWord("b", () -> {
                    throw new RuntimeException();
                })).isEmpty();
            }
        }
    }

    @Nested
    class FetchElementNode {

        @Nested
        class ByChar {
            @Test
            void return_empty_when_not_start_with_char() {
                SourceCode code = new SourceCode("ab");
                Optional<TestNode> optionalTestNode = code.fetchElementNode(BY_CHAR, 'b', 'b',
                        () -> code.escapedPop(NO_ESCAPE), TestNode::new);
                assertThat(optionalTestNode).isEmpty();
                assertThat(code.nextPosition()).isEqualTo(0);
            }

            @Test
            void return_node_match_start_and_end_node_with_correct_position() {
                SourceCode code = new SourceCode("  a12345b");

                TestNode testNode = code.fetchElementNode(BY_CHAR, 'a', 'b',
                        () -> code.escapedPop(NO_ESCAPE), TestNode::new).get();

                assertThat(testNode.getContent()).isEqualTo(asList('1', '2', '3', '4', '5'));
                assertThat(testNode.getPositionBegin()).isEqualTo(2);
            }

            @Test
            void raise_error_when_node_not_finish() {
                SourceCode code = new SourceCode("  a12345");

                assertThat(assertThrows(SyntaxException.class, () -> code.fetchElementNode(BY_CHAR, 'a', 'b',
                        () -> code.escapedPop(NO_ESCAPE), TestNode::new))).hasMessageContaining("should end with `b`");
            }
        }

        @Nested
        class ByNode {
            @Test
            void return_empty_when_not_start_with_char() {
                SourceCode code = new SourceCode("ab");
                Optional<TestNode> optionalTestNode = code.fetchElementNode(BY_NODE, 'b', 'b',
                        () -> code.escapedPop(NO_ESCAPE), TestNode::new);
                assertThat(optionalTestNode).isEmpty();
                assertThat(code.nextPosition()).isEqualTo(0);
            }

            @Test
            void return_node_match_start_and_end_node_with_correct_position() {
                SourceCode code = new SourceCode("  a1, 2, 3, 4, 5b");

                TestNode testNode = code.fetchElementNode(BY_NODE, 'a', 'b',
                        () -> new TestNode(code.escapedPop(NO_ESCAPE)), TestNode::new).get();

                assertThat(testNode.getPositionBegin()).isEqualTo(2);
                assertThat(testNode.getContent()).isEqualTo(
                        asList(new TestNode('1'),
                                new TestNode('2'),
                                new TestNode('3'),
                                new TestNode('4'),
                                new TestNode('5')));
            }

            @Test
            void raise_error_when_node_not_finish() {
                SourceCode code = new SourceCode("  a1, 2, 3, 4, 5");

                assertThat(assertThrows(SyntaxException.class, () -> code.fetchElementNode(BY_NODE, 'a', 'b',
                        () -> new TestNode(code.escapedPop(NO_ESCAPE)), TestNode::new))).hasMessageContaining("should end with `b`");
            }

            @Test
            void support_tail_comma() {
                SourceCode code = new SourceCode("  a1, b");

                TestNode testNode = code.fetchElementNode(BY_NODE, 'a', 'b',
                        () -> new TestNode(code.escapedPop(NO_ESCAPE)), TestNode::new).get();

                assertThat(testNode.getPositionBegin()).isEqualTo(2);
                assertThat(testNode.getContent()).isEqualTo(asList(new TestNode('1')));
            }
        }
    }

    @Nested
    class TryFetch {

        @Test
        void fetch_result_and_move_to_next() {
            SourceCode code = new SourceCode("ab");
            assertThat(code.tryFetch(() -> code.popWord("a")).get().getContent()).isEqualTo("a");

            assertThat(code.escapedPop(NO_ESCAPE)).isEqualTo('b');
        }

        @Test
        void should_not_move_position_when_fetch_empty() {
            SourceCode code = new SourceCode("ab");
            assertThat(code.tryFetch(() -> code.popWord("b"))).isEmpty();

            assertThat(code.escapedPop(NO_ESCAPE)).isEqualTo('a');
        }
    }

    @Nested
    class RepeatWords {

        @Test
        void pop_same_word() {
            assertThat(new SourceCode("aaa").repeatWords("a", i -> i).get()).isEqualTo(3);
            assertThat(new SourceCode(" \r\n\taaa").repeatWords("a", i -> i).get()).isEqualTo(3);
            assertThat(new SourceCode("aa").repeatWords("a", i -> i).get()).isEqualTo(2);
        }

        @Test
        void allow_blank_between_word() {
            assertThat(new SourceCode("a \n\r\ta").repeatWords("a", i -> i).get()).isEqualTo(2);
        }
    }

    @Nested
    class NextPosition {

        @Test
        void should_skip_blank_return_position() {
            assertThat(new SourceCode(" \n\r\ta").nextPosition()).isEqualTo(4);
        }
    }

    @Nested
    class EndOfLine {

        @Test
        void at_end_of_code() {
            assertThat(new SourceCode("").isEndOfLine()).isTrue();
        }

        @Test
        void space_before_change_line() {
            assertThat(new SourceCode("\n").isEndOfLine()).isTrue();
            assertThat(new SourceCode(" \n").isEndOfLine()).isTrue();
            assertThat(new SourceCode("\t\n").isEndOfLine()).isTrue();
            assertThat(new SourceCode(" \t\r\n").isEndOfLine()).isTrue();
        }
    }

    @Nested
    class TokenMatcher {
       
    }
}