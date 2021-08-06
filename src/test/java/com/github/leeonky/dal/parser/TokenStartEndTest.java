package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.token.SourceCode;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.parser.SourceCodeMatcher.CHARACTER;
import static com.github.leeonky.dal.parser.SourceCodeMatcher.OPERATOR;
import static com.github.leeonky.dal.parser.TokenStartEnd.*;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class TokenStartEndTest {

    private ParseContext givenParseContextBySourceCode(String sourceCode) {
        return new ParseContext(new SourceCode(sourceCode), null);
    }

    @Nested
    class Include {

        private boolean includedMatches(SourceCodeMatcher character, ParseContext context) {
            return included(character).matches(context);
        }

        @Test
        void should_return_true_when_code_matches() {
            ParseContext context = givenParseContextBySourceCode("a");

            assertThat(includedMatches(CHARACTER('a'), context)).isTrue();
        }

        @Test
        void should_seek_source_code_to_next_char() {
            ParseContext context = givenParseContextBySourceCode("ab");

            includedMatches(CHARACTER('a'), context);

            assertThat(context.sourceCode.currentChar()).isEqualTo('b');
        }

        @Test
        void should_include_matches_char_in_code() {
            ParseContext context = givenParseContextBySourceCode("a");

            includedMatches(CHARACTER('a'), context);

            assertThat(context.content).containsExactly('a');
        }

        @Test
        void should_return_false_when_not_match_and_keep_origin_status() {
            ParseContext context = givenParseContextBySourceCode("a");

            assertThat(includedMatches(CHARACTER('x'), context)).isFalse();
        }
    }

    @Nested
    class Exclude {

        private boolean excludedMatches(SourceCodeMatcher character, ParseContext context) {
            return excluded(character).matches(context);
        }

        @Test
        void should_return_true_when_code_matches() {
            ParseContext context = givenParseContextBySourceCode("a");

            assertThat(excludedMatches(CHARACTER('a'), context)).isTrue();
        }

        @Test
        void should_seek_source_code_to_next_char() {
            ParseContext context = givenParseContextBySourceCode("ab");

            excludedMatches(CHARACTER('a'), context);

            assertThat(context.sourceCode.currentChar()).isEqualTo('b');
        }

        @Test
        void should_not_include_matches_char_in_code() {
            ParseContext context = givenParseContextBySourceCode("a");

            excludedMatches(CHARACTER('a'), context);

            assertThat(context.content).isEmpty();
        }

        @Test
        void should_return_false_when_not_match_and_keep_origin_status() {
            ParseContext context = givenParseContextBySourceCode("a");

            assertThat(excludedMatches(CHARACTER('x'), context)).isFalse();
        }
    }

    @Nested
    class Before {

        private boolean beforeMatches(SourceCodeMatcher character, ParseContext context) {
            return before(character).matches(context);
        }

        @Test
        void should_return_true_when_code_matches() {
            ParseContext context = givenParseContextBySourceCode("a");

            assertThat(beforeMatches(CHARACTER('a'), context)).isTrue();
        }

        @Test
        void should_keep_matches_source_code_position() {
            ParseContext context = givenParseContextBySourceCode("ab");

            beforeMatches(CHARACTER('a'), context);

            assertThat(context.sourceCode.currentChar()).isEqualTo('a');
        }

        @Test
        void should_not_include_matches_char_in_code() {
            ParseContext context = givenParseContextBySourceCode("a");

            beforeMatches(CHARACTER('a'), context);

            assertThat(context.content).isEmpty();
        }

        @Test
        void should_return_false_when_not_match_and_keep_origin_status() {
            ParseContext context = givenParseContextBySourceCode("a");

            assertThat(beforeMatches(CHARACTER('x'), context)).isFalse();
        }
    }

    @Nested
    class EndOfCode {

        private boolean matchEndOfCode(ParseContext context) {
            return TokenStartEnd.END_OF_CODE.matches(context);
        }

        @Test
        void should_return_true_when_no_more_code() {
            ParseContext context = givenParseContextBySourceCode("");

            assertThat(matchEndOfCode(context)).isTrue();
        }

        @Test
        void should_not_include_matches_char_in_code() {
            ParseContext context = givenParseContextBySourceCode("");

            matchEndOfCode(context);

            assertThat(context.content).isEmpty();
        }

        @Test
        void should_return_false_when_has_any_code() {
            ParseContext context = givenParseContextBySourceCode("any code");

            assertThat(matchEndOfCode(context)).isFalse();
        }
    }

    @Nested
    class Always {

    }

    @Nested
    class LogicOr {

        @Test
        void support_logic_or() {
            ParseContext context = givenParseContextBySourceCode("a");

            assertThat(END_OF_CODE.or(included(CHARACTER('a'))).matches(context)).isTrue();
        }
    }

    @Nested
    class ThrowException {

        @Test
        void should_throw_exception_with_message_when_no_more_code() {
            ParseContext context = givenParseContextBySourceCode("");

            assertThat(assertThrows(SyntaxException.class, () ->
                    excluded(OPERATOR).orThrow("error").matches(context)))
                    .hasMessage("error")
                    .hasFieldOrPropertyWithValue("position", 0);
        }
    }
}
