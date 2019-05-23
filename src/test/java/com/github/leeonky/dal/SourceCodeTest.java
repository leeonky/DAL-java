package com.github.leeonky.dal;

import com.github.leeonky.dal.token.Token;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static com.github.leeonky.dal.token.Token.*;
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

        private void assertGetToken(String sourceCode, Token... tokens) {
            SourceCode sourceCode1 = new SourceCode(sourceCode);
            assertThat(IntStream.range(0, tokens.length).boxed()
                    .map(i -> sourceCode1.getToken())
                    .collect(Collectors.toList())).containsOnly(tokens);
        }

        @Test
        void number_token() {
            assertGetToken("1", constValueToken(new BigDecimal(1)));
            assertGetToken("11", constValueToken(new BigDecimal(11)));
        }

        @Nested
        class TypeToken {

            @Test
            void single_type_token() {
                assertGetToken("ab", typeToken("ab"));
            }

            @Test
            void two_single_tokens() {
                assertGetToken("ab   cd", typeToken("ab"), typeToken("cd"));
            }

            @Test
            void nested_type_token() {
                assertGetToken("a.b", typeToken("a.b"));
            }
        }

        @Nested
        class PropertyToken {

            @Test
            void single_property_token() {
                assertGetToken(".a", propertyToken(".a"));
            }

            @Test
            void property_chain_token() {
                assertGetToken(".a.x", propertyToken(".a.x"));
            }
        }
    }
}