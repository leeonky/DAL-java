package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.token.SourceCode;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.parser.NewTokenFactory.CONST_STRING_TOKEN;
import static com.github.leeonky.dal.parser.ParsingContext.*;
import static com.github.leeonky.dal.parser.TokenContentInString.ALL_CHARACTERS;
import static com.github.leeonky.dal.parser.TokenStartEnd.before;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class TokenStartEndTest {

    private SourceCode sourceCode;

    private ParsingContext givenParseContextBySourceCode(String sourceCode) {
        return new ParsingContext(this.sourceCode = new SourceCode(sourceCode), null);
    }

    private Object getParsedCode(ParsingContext context) {
        return context.parseToken(excluded(ANY_CHARACTERS), ALL_CHARACTERS, before(ANY_CHARACTERS), CONST_STRING_TOKEN).getValue();
    }

    @Nested
    class Include {

        private boolean includedMatches(SourceCodeMatcher character, ParsingContext context) {
            return ParsingContext.included(character).matches(context);
        }

        @Test
        void should_return_true_when_code_matches() {
            ParsingContext context = givenParseContextBySourceCode("a");

            assertThat(includedMatches(CHARACTER('a'), context)).isTrue();
        }

        @Test
        void should_seek_source_code_to_next_char() {
            ParsingContext context = givenParseContextBySourceCode("ab");

            includedMatches(CHARACTER('a'), context);

            assertThat(sourceCode.currentChar()).isEqualTo('b');
        }

        @Test
        void should_include_matches_char_in_code() {
            ParsingContext context = givenParseContextBySourceCode("abc");

            includedMatches(CHARACTER('a'), context);

            assertThat(getParsedCode(context)).isEqualTo("a");
        }

        @Test
        void should_return_false_when_not_match_and_keep_origin_status() {
            ParsingContext context = givenParseContextBySourceCode("a");

            assertThat(includedMatches(CHARACTER('x'), context)).isFalse();
        }
    }

    @Nested
    class Exclude {

        private boolean excludedMatches(SourceCodeMatcher character, ParsingContext context) {
            return excluded(character).matches(context);
        }

        @Test
        void should_return_true_when_code_matches() {
            ParsingContext context = givenParseContextBySourceCode("a");

            assertThat(excludedMatches(CHARACTER('a'), context)).isTrue();
        }

        @Test
        void should_seek_source_code_to_next_char() {
            ParsingContext context = givenParseContextBySourceCode("ab");

            excludedMatches(CHARACTER('a'), context);

            assertThat(sourceCode.currentChar()).isEqualTo('b');
        }

        @Test
        void should_not_include_matches_char_in_code() {
            ParsingContext context = givenParseContextBySourceCode("abc");

            excludedMatches(CHARACTER('a'), context);

            assertThat(getParsedCode(context)).isEqualTo("");
        }

        @Test
        void should_return_false_when_not_match_and_keep_origin_status() {
            ParsingContext context = givenParseContextBySourceCode("a");

            assertThat(excludedMatches(CHARACTER('x'), context)).isFalse();
        }
    }

    @Nested
    class Before {

        private boolean beforeMatches(SourceCodeMatcher character, ParsingContext context) {
            return before(character).matches(context);
        }

        @Test
        void should_return_true_when_code_matches() {
            ParsingContext context = givenParseContextBySourceCode("a");

            assertThat(beforeMatches(CHARACTER('a'), context)).isTrue();
        }

        @Test
        void should_keep_matches_source_code_position() {
            ParsingContext context = givenParseContextBySourceCode("ab");

            beforeMatches(CHARACTER('a'), context);

            assertThat(sourceCode.currentChar()).isEqualTo('a');
        }

        @Test
        void should_not_include_matches_char_in_code() {
            ParsingContext context = givenParseContextBySourceCode("abc");

            beforeMatches(CHARACTER('a'), context);

            assertThat(getParsedCode(context)).isEqualTo("");
        }

        @Test
        void should_return_false_when_not_match_and_keep_origin_status() {
            ParsingContext context = givenParseContextBySourceCode("a");

            assertThat(beforeMatches(CHARACTER('x'), context)).isFalse();
        }
    }

    @Nested
    class EndOfCode {

        private boolean matchEndOfCode(ParsingContext context) {
            return ParsingContext.END_OF_CODE.matches(context);
        }

        @Test
        void should_return_true_when_no_more_code() {
            ParsingContext context = givenParseContextBySourceCode("");

            assertThat(matchEndOfCode(context)).isTrue();
        }

        @Test
        void should_return_false_when_has_any_code() {
            ParsingContext context = givenParseContextBySourceCode("any code");

            assertThat(matchEndOfCode(context)).isFalse();
        }
    }

    @Nested
    class LogicOr {

        @Test
        void support_logic_or() {
            ParsingContext context = givenParseContextBySourceCode("a");

            assertThat(ParsingContext.END_OF_CODE.or(ParsingContext.included(CHARACTER('a'))).matches(context)).isTrue();
        }
    }

    @Nested
    class ThrowException {

        @Test
        void should_throw_exception_with_message_when_no_more_code() {
            ParsingContext context = givenParseContextBySourceCode("");

            assertThat(assertThrows(SyntaxException.class, () ->
                    excluded(OPERATOR).orThrow("error").matches(context)))
                    .hasMessage("error")
                    .hasFieldOrPropertyWithValue("position", 0);
        }
    }
}
