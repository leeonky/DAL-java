package com.github.leeonky.interpreter;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Optional;
import java.util.function.BiPredicate;

import static com.github.leeonky.interpreter.Notation.notation;
import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;

class SourceCodeTest {

    public static final HashMap<String, Character> NO_ESCAPE = new HashMap<>();
    public static final BiPredicate<String, Integer> ONE_CHAR_TOKEN = (c1, c2) -> true;
    public static final BiPredicate<String, Integer> UNLIMITED_ENDING = (c1, c2) -> false;

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
            assertThat(new SourceCode("ab").startsWith(notation("a"))).isTrue();
        }

        @Test
        void not_start_with_given_word() {
            assertThat(new SourceCode("ab").startsWith(notation("b"))).isFalse();
        }

        @Test
        void trim_blank_and_start_with_given_word() {
            assertThat(new SourceCode(" \n\r\tab").startsWith(notation("a"))).isTrue();
        }

        @Test
        void starts_with_after_pop() {
            SourceCode sourceCode = new SourceCode("x \n\r\tab");
            sourceCode.popChar(NO_ESCAPE);
            assertThat(sourceCode.startsWith(notation("a"))).isTrue();
        }
    }

    @Nested
    class EscapedPop {

        @Test
        void pop_up_when_no_escape() {
            SourceCode sourceCode = new SourceCode("a");
            assertThat(sourceCode.popChar(NO_ESCAPE)).isEqualTo('a');
            assertThat(sourceCode.hasCode()).isFalse();
        }

        @Test
        void pop_up_when_escape_not_match() {
            SourceCode sourceCode = new SourceCode("a");
            assertThat(sourceCode.popChar(new EscapeChars().escape("b", 'a'))).isEqualTo('a');
            assertThat(sourceCode.hasCode()).isFalse();
        }

        @Test
        void pop_up_when_escaped() {
            SourceCode sourceCode = new SourceCode("aabbx");
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
            assertThat(new SourceCode("a").isBeginning()).isTrue();
            assertThat(new SourceCode(" ").isBeginning()).isTrue();
            assertThat(new SourceCode("").isBeginning()).isTrue();
        }

        @Test
        void also_is_beginning_even_pop_blanks() {
            SourceCode sourceCode = new SourceCode(" \n\r\t");
            sourceCode.popChar(NO_ESCAPE);
            sourceCode.popChar(NO_ESCAPE);
            sourceCode.popChar(NO_ESCAPE);
            sourceCode.popChar(NO_ESCAPE);
            assertThat(sourceCode.isBeginning()).isTrue();
        }
    }

    @Nested
    class ThrowSyntaxException {

        @Test
        void throw_exception_with_position_info() {
            SourceCode sourceCode = new SourceCode("abc");
            sourceCode.popChar(NO_ESCAPE);
            assertThat(sourceCode.syntaxError("test", 1).show("abc")).isEqualTo("abc\n  ^");
        }
    }

    @Nested
    class PopWord {

        @Test
        void pop_matched_word() {
            SourceCode code = new SourceCode("ab");
            code.popChar(NO_ESCAPE);
            Token token = code.popWord(notation("b")).get();
            assertThat(token.getContent()).isEqualTo("b");
            assertThat(token.getPosition()).isEqualTo(1);
        }

        @Test
        void pop_not_matched_word() {
            SourceCode code = new SourceCode("ab");
            assertThat(code.popWord(notation("b"))).isEmpty();
            assertThat(code.popChar(NO_ESCAPE)).isEqualTo('a');
        }

        @Test
        void trim_before_pop_word() {
            SourceCode code = new SourceCode(" \n\t\rb");

            assertThat(code.popWord(notation("b")).get().getContent()).isEqualTo("b");
        }

        @Nested
        class ConditionalPop {

            @Test
            void non_matched_word() {
                SourceCode code = new SourceCode("ab");
                assertThat(code.popWord(notation("a"), () -> false)).isEmpty();

                assertThat(code.nextPosition()).isEqualTo(0);
            }

            @Test
            void predicate_should_not_called_when_word_not_matched() {
                SourceCode code = new SourceCode("ab");
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
            SourceCode code = new SourceCode("ab");
            assertThat(code.tryFetch(() -> code.popWord(notation("a"))).get().getContent()).isEqualTo("a");

            assertThat(code.popChar(NO_ESCAPE)).isEqualTo('b');
        }

        @Test
        void should_not_move_position_when_fetch_empty() {
            SourceCode code = new SourceCode("ab");
            assertThat(code.tryFetch(() -> code.popWord(notation("b")))).isEmpty();

            assertThat(code.popChar(NO_ESCAPE)).isEqualTo('a');
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
    class TokenMatcherS {

        @Test
        void return_empty_when_not_match_opening_char() {
            SourceCode sourceCode = new SourceCode(" notStartsWithA");
            Optional<Token> optionalToken = SourceCode.tokenScanner(c -> c.equals('a'), new HashSet<>(),
                    false, ONE_CHAR_TOKEN, token -> true).scan(sourceCode);

            assertThat(optionalToken).isEmpty();
            assertThat(sourceCode.popChar(NO_ESCAPE)).isEqualTo('n');
        }

        @Test
        void return_token_with_content_and_position() {
            SourceCode sourceCode = new SourceCode(" abc");

            Token token = SourceCode.tokenScanner(c -> c.equals('a'), new HashSet<>(),
                    false, ONE_CHAR_TOKEN, token1 -> true).scan(sourceCode).get();

            assertThat(token.getContent()).isEqualTo("a");
            assertThat(token.getPosition()).isEqualTo(1);
            assertThat(sourceCode.popChar(NO_ESCAPE)).isEqualTo('b');
        }

        @Test
        void should_append_a_char_when_no_matter_end_with_when_trim() {
            SourceCode sourceCode = new SourceCode(" abc");

            Token token = SourceCode.tokenScanner(c -> c.equals('a'), new HashSet<>(),
                    true, ONE_CHAR_TOKEN, token1 -> true).scan(sourceCode).get();

            assertThat(token.getContent()).isEqualTo("b");
            assertThat(token.getPosition()).isEqualTo(1);
            assertThat(sourceCode.popChar(NO_ESCAPE)).isEqualTo('c');
        }

        @Test
        void trim_start_blank() {
            SourceCode sourceCode = new SourceCode(" a bc");

            Token token = SourceCode.tokenScanner(c -> c.equals('a'), new HashSet<>(),
                    true, ONE_CHAR_TOKEN, token1 -> true).scan(sourceCode).get();

            assertThat(token.getContent()).isEqualTo("b");
            assertThat(token.getPosition()).isEqualTo(1);
            assertThat(sourceCode.popChar(NO_ESCAPE)).isEqualTo('c');
        }

        @Test
        void should_return_empty_content_token_when_start_char_is_at_the_end_of_code() {
            SourceCode sourceCode = new SourceCode(" a ");

            Token token = SourceCode.tokenScanner(c -> c.equals('a'), new HashSet<>(),
                    true, ONE_CHAR_TOKEN, token1 -> true).scan(sourceCode).get();

            assertThat(token.getContent()).isEqualTo("");
            assertThat(token.getPosition()).isEqualTo(1);
            assertThat(sourceCode.hasCode()).isFalse();
        }

        @Test
        void should_return_all_content_when_got_the_end_of_code() {
            SourceCode sourceCode = new SourceCode(" abc");

            Token token = SourceCode.tokenScanner(c -> c.equals('a'), new HashSet<>(),
                    false, UNLIMITED_ENDING, token1 -> true).scan(sourceCode).get();

            assertThat(token.getContent()).isEqualTo("abc");
            assertThat(token.getPosition()).isEqualTo(1);
            assertThat(sourceCode.hasCode()).isFalse();
        }

        @Test
        void should_return_content_when_before_closing_char() {
            SourceCode sourceCode = new SourceCode(" abc");

            Token token = SourceCode.tokenScanner(c -> c.equals('a'), new HashSet<>(),
                    false, (code, position) -> code.charAt(position) == 'c', token1 -> true).scan(sourceCode).get();

            assertThat(token.getContent()).isEqualTo("ab");
            assertThat(token.getPosition()).isEqualTo(1);
            assertThat(sourceCode.popChar(NO_ESCAPE)).isEqualTo('c');
        }

        @Test
        void return_empty_when_excluded() {
            SourceCode sourceCode = new SourceCode(" abc");
            Optional<Token> optionalToken = SourceCode.tokenScanner(c -> c.equals('a'), new HashSet<>(asList("a")),
                    false, ONE_CHAR_TOKEN, token -> true).scan(sourceCode);

            assertThat(optionalToken).isEmpty();
            assertThat(sourceCode.popChar(NO_ESCAPE)).isEqualTo('a');
        }

        @Test
        void return_empty_when_predicate_false() {
            SourceCode sourceCode = new SourceCode(" abc");

            Optional<Token> optionalToken = SourceCode.tokenScanner(c -> c.equals('a'), new HashSet<>(),
                    false, (code, position) -> code.charAt(position) == 'c', token -> {
                        assertThat(token.getContent()).isEqualTo("ab");
                        assertThat(token.getPosition()).isEqualTo(1);
                        return false;
                    }).scan(sourceCode);

            assertThat(optionalToken).isEmpty();
            assertThat(sourceCode.popChar(NO_ESCAPE)).isEqualTo('a');
        }

        @Test
        void return_by_delimiters() {
            SourceCode sourceCode = new SourceCode(" abc");

            Token token = SourceCode.tokenScanner(c -> c.equals('a'), new HashSet<>(),
                    false, new HashSet<>(asList('c'))).scan(sourceCode).get();

            assertThat(token.getContent()).isEqualTo("ab");
            assertThat(token.getPosition()).isEqualTo(1);
            assertThat(sourceCode.popChar(NO_ESCAPE)).isEqualTo('c');
        }
    }

    @Nested
    class OperatorMatchers {

        @Test
        void return_when_match_symbol() {
            TestOperator testOperator = new TestOperator();
            SourceCode sourceCode = new SourceCode(" +=");
            OperatorParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> operatorParser =
                    notation("+=").operatorParser(() -> testOperator);

            TestOperator testOperator2 = operatorParser.parse(new TestProcedure(sourceCode)).get();

            assertThat(testOperator2).isSameAs(testOperator);
            assertThat(testOperator2.getPosition()).isEqualTo(1);
        }

        @Test
        void return_empty_when_not_match() {
            SourceCode sourceCode = new SourceCode(" +=");
            OperatorParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> operatorParser =
                    notation("++").operatorParser(TestOperator::new);

            assertThat(operatorParser.parse(new TestProcedure(sourceCode))).isEmpty();
        }

        @Test
        void return_empty_when_predicate_false() {
            TestOperator testOperator = new TestOperator();
            SourceCode sourceCode = new SourceCode(" +=");
            TestProcedure testProcedure = new TestProcedure(sourceCode);

            OperatorParser<TestContext, TestNode, TestExpression, TestOperator, TestProcedure> operatorParser =
                    notation("+=").operatorParser(() -> testOperator, scanner1 -> {
                        assertThat(scanner1).isSameAs(testProcedure);
                        return false;
                    });

            assertThat(operatorParser.parse(testProcedure)).isEmpty();
        }
    }
}