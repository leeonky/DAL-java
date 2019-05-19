package com.github.leeonky.dal;

import com.github.leeonky.dal.token.Token;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static com.github.leeonky.dal.token.Token.constValue;
import static com.github.leeonky.dal.token.Token.token;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class SourceCodeTest {
    @Nested
    class GetToken {

        @Test
        void empty_code() {
            assertThrows(RuntimeException.class, () -> new SourceCode("").getToken());
            assertThrows(RuntimeException.class, () -> new SourceCode(" ").getToken());
        }

        @Test
        void single_token() {
            assertGetToken("ab", token("ab"));
        }

        @Test
        void with_end_char() {
            assertGetToken("a ", token("a"));
        }

        @Test
        void with_multi_tokens() {
            assertGetToken("ab   cd", token("ab"), token("cd"));
        }

        @Test
        void number_token() {
            assertGetToken("1", constValue(new BigDecimal(1)));
            assertGetToken("11", constValue(new BigDecimal(11)));
        }

        private void assertGetToken(String sourceCode, Token... tokens) {
            SourceCode sourceCode1 = new SourceCode(sourceCode);
            assertThat(IntStream.range(0, tokens.length).boxed()
                    .map(i -> sourceCode1.getToken())
                    .collect(Collectors.toList())).containsOnly(tokens);
        }
    }
}