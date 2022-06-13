package com.github.leeonky.interpreter;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Optional;

import static com.github.leeonky.interpreter.Notation.notation;
import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;

class SourceCodeTest extends BaseTest {

    public static final HashMap<String, Character> NO_ESCAPE = new HashMap<>();
    public static final TriplePredicate<String, Integer, Integer> ONE_CHAR_TOKEN = (c1, c2, s) -> s == 1;
    public static final TriplePredicate<String, Integer, Integer> UNLIMITED_ENDING = (c1, c2, s) -> false;

    @Nested
    class HasCode {

        @Test
        void should_has_code_when_position_not_end() {
            assertThat(BaseTest.createSourceCode("a").hasCode()).isTrue();
        }

        @Test
        void should_not_has_code_when_at_the_end() {
            assertThat(BaseTest.createSourceCode("").hasCode()).isFalse();
        }

        @Test
        void check_has_code_after_pop() {
            SourceCode code = BaseTest.createSourceCode("ab ");
            code.popChar(NO_ESCAPE);
            assertThat(code.hasCode()).isTrue();
            code.popChar(NO_ESCAPE);
            assertThat(code.hasCode()).isTrue();
            code.popChar(NO_ESCAPE);
            assertThat(code.hasCode()).isFalse();
        }
    }

    @Nested
    class StartsWithNotation {

        @Test
        void start_with_given_word() {
            assertThat(BaseTest.createSourceCode("ab").startsWith(notation("a"))).isTrue();
        }

        @Test
        void not_start_with_given_word() {
            assertThat(BaseTest.createSourceCode("ab").startsWith(notation("b"))).isFalse();
        }

        @Test
        void trim_blank_and_start_with_given_word() {
            assertThat(BaseTest.createSourceCode(" \n\r\tab").startsWith(notation("a"))).isTrue();
        }

        @Test
        void starts_with_after_pop() {
            SourceCode sourceCode = BaseTest.createSourceCode("x \n\r\tab");
            sourceCode.popChar(NO_ESCAPE);
            assertThat(sourceCode.startsWith(notation("a"))).isTrue();
        }
    }

    @Nested
    class StartWithString {

        @Test
        void start_with_given_word() {
            assertThat(BaseTest.createSourceCode("ab").startsWith("a")).isTrue();
        }

        @Test
        void not_start_with_given_word() {
            assertThat(BaseTest.createSourceCode("ab").startsWith("b")).isFalse();
        }

        @Test
        void should_not_trim_blank_when_start_with_given_word() {
            SourceCode sourceCode = BaseTest.createSourceCode("prefix \n\r\tab");
            sourceCode.popWord(notation("prefix"));
            assertThat(sourceCode.startsWith("a")).isFalse();
        }
    }

    @Nested
    class EscapedPop {

        @Test
        void pop_up_when_no_escape() {
            SourceCode sourceCode = BaseTest.createSourceCode("a");
            assertThat(sourceCode.popChar(NO_ESCAPE)).isEqualTo('a');
            assertThat(sourceCode.hasCode()).isFalse();
        }

        @Test
        void pop_up_when_escape_not_match() {
            SourceCode sourceCode = BaseTest.createSourceCode("a");
            assertThat(sourceCode.popChar(new EscapeChars().escape("b", 'a'))).isEqualTo('a');
            assertThat(sourceCode.hasCode()).isFalse();
        }

        @Test
        void pop_up_when_escaped() {
            SourceCode sourceCode = BaseTest.createSourceCode("aabbx");
            EscapeChars escape = new EscapeChars().escape("bb", 'a').escape("aa", 'b');
            assertThat(sourceCode.popChar(escape)).isEqualTo('b');
            assertThat(sourceCode.popChar(escape)).isEqualTo('a');
            assertThat(sourceCode.popChar(escape)).isEqualTo('x');
        }
    }

    @Nested
    class isBeginning {

        @Test
        void position_is_0() {
            assertThat(BaseTest.createSourceCode("a").isBeginning()).isTrue();
            assertThat(BaseTest.createSourceCode(" ").isBeginning()).isTrue();
            assertThat(BaseTest.createSourceCode("").isBeginning()).isTrue();
        }

        @Test
        void also_is_beginning_even_pop_blanks() {
            SourceCode sourceCode = BaseTest.createSourceCode(" \n\r\txx");
            assertThat(sourceCode.isBeginning()).isTrue();
        }
    }

    @Nested
    class ThrowSyntaxException {

        @Test
        void throw_exception_with_position_info() {
            SourceCode sourceCode = BaseTest.createSourceCode("abc");
            sourceCode.popChar(NO_ESCAPE);
            assertThat(sourceCode.syntaxError("test", 1).show("abc")).isEqualTo("abc\n  ^");
        }
    }

    @Nested
    class PopWord {

        @Test
        void pop_matched_word() {
            SourceCode code = BaseTest.createSourceCode("ab");
            code.popChar(NO_ESCAPE);
            Token token = code.popWord(notation("b")).get();
            assertThat(token.getContent()).isEqualTo("b");
            assertThat(token.getPosition()).isEqualTo(1);
        }

        @Test
        void pop_not_matched_word() {
            SourceCode code = BaseTest.createSourceCode("ab");
            assertThat(code.popWord(notation("b"))).isEmpty();
            assertThat(code.popChar(NO_ESCAPE)).isEqualTo('a');
        }

        @Test
        void trim_before_pop_word() {
            SourceCode code = BaseTest.createSourceCode(" \n\t\rb");

            assertThat(code.popWord(notation("b")).get().getContent()).isEqualTo("b");
        }

        @Nested
        class ConditionalPop {

            @Test
            void non_matched_word() {
                SourceCode code = BaseTest.createSourceCode("ab");
                assertThat(code.popWord(notation("a"), () -> false)).isEmpty();

                assertThat(code.nextPosition()).isEqualTo(0);
            }

            @Test
            void predicate_should_not_called_when_word_not_matched() {
                SourceCode code = BaseTest.createSourceCode("ab");
                assertThat(code.popWord(notation("b"), () -> {
                    throw new RuntimeException();
                })).isEmpty();
            }
        }
    }

    @Nested
    class TryFetch {

        @Test
        void fetch_result_and_move_to_next() {
            SourceCode code = BaseTest.createSourceCode("ab");
            assertThat(code.tryFetch(() -> code.popWord(notation("a"))).get().getContent()).isEqualTo("a");

            assertThat(code.popChar(NO_ESCAPE)).isEqualTo('b');
        }

        @Test
        void should_not_move_position_when_fetch_empty() {
            SourceCode code = BaseTest.createSourceCode("ab");
            assertThat(code.tryFetch(() -> code.popWord(notation("b")))).isEmpty();

            assertThat(code.popChar(NO_ESCAPE)).isEqualTo('a');
        }
    }

    @Nested
    class NextPosition {

        @Test
        void should_skip_blank_return_position() {
            assertThat(BaseTest.createSourceCode(" \n\r\ta").nextPosition()).isEqualTo(4);
        }
    }

    @Nested
    class EndOfLine {

        @Test
        void at_end_of_code() {
            assertThat(BaseTest.createSourceCode("").isEndOfLine()).isTrue();
        }

        @Test
        void space_before_change_line() {
            testEndOfLine("\n");
            testEndOfLine(" \n");
            testEndOfLine("\t\n");
            testEndOfLine(" \t\r\n");
        }

        private void testEndOfLine(String s) {
            SourceCode sourceCode = BaseTest.createSourceCode("a" + s);
            sourceCode.popChar(new HashMap<>());
            assertThat(sourceCode.isEndOfLine()).isTrue();
        }
    }

    @Nested
    class TokenMatcherS {

        @Test
        void return_empty_when_not_match_opening_char() {
            SourceCode sourceCode = BaseTest.createSourceCode(" notStartsWithA");
            Optional<Token> optionalToken = SourceCode.tokenScanner(c -> c.equals('a'), new HashSet<>(),
                    false, ONE_CHAR_TOKEN, token -> true).scan(sourceCode);

            assertThat(optionalToken).isEmpty();
            assertThat(sourceCode.popChar(NO_ESCAPE)).isEqualTo('n');
        }

        @Test
        void return_token_with_content_and_position() {
            SourceCode sourceCode = BaseTest.createSourceCode(" abc");

            Token token = SourceCode.tokenScanner(c -> c.equals('a'), new HashSet<>(),
                    false, ONE_CHAR_TOKEN, token1 -> true).scan(sourceCode).get();

            assertThat(token.getContent()).isEqualTo("a");
            assertThat(token.getPosition()).isEqualTo(1);
            assertThat(sourceCode.popChar(NO_ESCAPE)).isEqualTo('b');
        }

        @Test
        void should_append_a_char_when_no_matter_end_with_when_trim() {
            SourceCode sourceCode = BaseTest.createSourceCode(" abc");

            Token token = SourceCode.tokenScanner(c -> c.equals('a'), new HashSet<>(),
                    true, ONE_CHAR_TOKEN, token1 -> true).scan(sourceCode).get();

            assertThat(token.getContent()).isEqualTo("b");
            assertThat(token.getPosition()).isEqualTo(1);
            assertThat(sourceCode.popChar(NO_ESCAPE)).isEqualTo('c');
        }

        @Test
        void trim_start_blank() {
            SourceCode sourceCode = BaseTest.createSourceCode(" a bc");

            Token token = SourceCode.tokenScanner(c -> c.equals('a'), new HashSet<>(),
                    true, ONE_CHAR_TOKEN, token1 -> true).scan(sourceCode).get();

            assertThat(token.getContent()).isEqualTo("b");
            assertThat(token.getPosition()).isEqualTo(1);
            assertThat(sourceCode.popChar(NO_ESCAPE)).isEqualTo('c');
        }

        @Test
        void should_return_empty_content_token_when_start_char_is_at_the_end_of_code() {
            SourceCode sourceCode = BaseTest.createSourceCode(" a ");

            Token token = SourceCode.tokenScanner(c -> c.equals('a'), new HashSet<>(),
                    true, ONE_CHAR_TOKEN, token1 -> true).scan(sourceCode).get();

            assertThat(token.getContent()).isEqualTo("");
            assertThat(token.getPosition()).isEqualTo(1);
            assertThat(sourceCode.hasCode()).isFalse();
        }

        @Test
        void should_return_all_content_when_got_the_end_of_code() {
            SourceCode sourceCode = BaseTest.createSourceCode(" abc");

            Token token = SourceCode.tokenScanner(c -> c.equals('a'), new HashSet<>(),
                    false, UNLIMITED_ENDING, token1 -> true).scan(sourceCode).get();

            assertThat(token.getContent()).isEqualTo("abc");
            assertThat(token.getPosition()).isEqualTo(1);
            assertThat(sourceCode.hasCode()).isFalse();
        }

        @Test
        void should_return_content_when_before_closing_char() {
            SourceCode sourceCode = BaseTest.createSourceCode(" abc");
            Token token = SourceCode.tokenScanner(c -> c.equals('a'), new HashSet<>(),
                    false, (code, position, size) -> code.charAt(position) == 'c', token1 -> true).scan(sourceCode).get();

            assertThat(token.getContent()).isEqualTo("ab");
            assertThat(token.getPosition()).isEqualTo(1);
            assertThat(sourceCode.popChar(NO_ESCAPE)).isEqualTo('c');
        }

        @Test
        void return_empty_when_excluded() {
            SourceCode sourceCode = BaseTest.createSourceCode(" abc");
            Optional<Token> optionalToken = SourceCode.tokenScanner(c -> c.equals('a'), new HashSet<>(asList("a")),
                    false, ONE_CHAR_TOKEN, token -> true).scan(sourceCode);

            assertThat(optionalToken).isEmpty();
            assertThat(sourceCode.popChar(NO_ESCAPE)).isEqualTo('a');
        }

        @Test
        void return_empty_when_predicate_false() {
            SourceCode sourceCode = BaseTest.createSourceCode(" abc");

            Optional<Token> optionalToken = SourceCode.tokenScanner(c -> c.equals('a'), new HashSet<>(),
                    false, (code, position, size) -> code.charAt(position) == 'c', token -> {
                        assertThat(token.getContent()).isEqualTo("ab");
                        assertThat(token.getPosition()).isEqualTo(1);
                        return false;
                    }).scan(sourceCode);

            assertThat(optionalToken).isEmpty();
            assertThat(sourceCode.popChar(NO_ESCAPE)).isEqualTo('a');
        }

        @Test
        void return_by_delimiters() {
            SourceCode sourceCode = BaseTest.createSourceCode(" abc");

            Token token = SourceCode.tokenScanner(c -> c.equals('a'), new HashSet<>(),
                    false, new HashSet<>(asList('c'))).scan(sourceCode).get();

            assertThat(token.getContent()).isEqualTo("ab");
            assertThat(token.getPosition()).isEqualTo(1);
            assertThat(sourceCode.popChar(NO_ESCAPE)).isEqualTo('c');
        }
    }

    @Nested
    class CodeBefore {

        @Test
        void return_code_before_notation() {
            SourceCode sourceCode = createSourceCodeWithBlank(" abc");
            assertThat(sourceCode.codeBefore(notation("bc"))).isEqualTo(" a");
        }

        @Test
        void return_last_code_when_not_matches() {
            SourceCode sourceCode = createSourceCodeWithBlank(" abc");
            assertThat(sourceCode.codeBefore(notation("x"))).isEqualTo(" abc");
        }

        @Test
        void return_empty_when_no_code() {
            SourceCode sourceCode = createSourceCode("");
            assertThat(sourceCode.codeBefore(notation("x"))).isEqualTo("");
        }
    }

    @Nested
    class TrimStartComments {

        @Test
        void should_trim_start_comments() {
            SourceCode sourceCode = SourceCode.createSourceCode("//comments\nx", asList(notation("//")));

            assertThat(sourceCode.popChar(new HashMap<>())).isEqualTo('x');
        }
    }

    @Nested
    class ToNodeParser {

        @Test
        void should_return_empty_node_parser_when_empty_token_scanner() {
            TestProcedure procedure = givenProcedureWithCode("not match");

            TokenScanner<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> scanner =
                    SourceCode.tokenScanner(c -> c.equals('a'), new HashSet<>(), false, ONE_CHAR_TOKEN, token -> true);

            assertThat(scanner.nodeParser(t -> new TestNode(t.getContent())).parse(procedure)).isEmpty();
        }

        @Test
        void should_return_present_node_parser_when_present_token_scanner_with_given_token_content_and_position() {
            TestProcedure procedure = givenProcedureWithCode(" a");

            TokenScanner<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> scanner =
                    SourceCode.tokenScanner(c -> c.equals('a'), new HashSet<>(), false, ONE_CHAR_TOKEN, token -> true);

            TestNode testNode = scanner.nodeParser(t -> new TestNode(t.getContent())).parse(procedure).get();
            assertThat(testNode.getContent()).isEqualTo("a");
            assertThat(testNode.getPositionBegin()).isEqualTo(1);
        }

        @Test
        void should_return_mandatory_node_parser_when_mandatory_token_scanner_with_given_token_content_and_position() {
            TestProcedure procedure = givenProcedureWithCode(" abc");

            TokenScanner.Mandatory<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> scanner =
                    SourceCode.tokenScanner(false, (code, position, size) -> size == 1);

            TestNode testNode = scanner.nodeParser(t -> new TestNode(t.getContent())).parse(procedure);
            assertThat(testNode.getContent()).isEqualTo("a");
            assertThat(testNode.getPositionBegin()).isEqualTo(1);
        }
    }
}