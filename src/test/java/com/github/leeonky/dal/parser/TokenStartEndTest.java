package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.token.SourceCode;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.parser.SourceCodeMatcher.CHARACTER;
import static com.github.leeonky.dal.parser.TokenStartEnd.*;
import static org.assertj.core.api.Assertions.assertThat;

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
}
