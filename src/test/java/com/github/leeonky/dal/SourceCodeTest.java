package com.github.leeonky.dal;

import com.github.leeonky.dal.token.Token;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

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
            List<Token> actualTokens = new ArrayList<>();
            SourceCode sourceCode1 = new SourceCode(sourceCode);
            while (!sourceCode1.isEnd())
                actualTokens.add(sourceCode1.getToken());
            assertThat(actualTokens).containsOnly(tokens);
        }

        @Nested
        class NumberToken {
            @Test
            void number_token() {
                assertGetToken("1", numebrToken(new BigDecimal(1)));
                assertGetToken("11", numebrToken(new BigDecimal(11)));
            }
        }

        @Nested
        class WordToken {

            @Test
            void single_word_token() {
                assertGetToken("ab", wordToken("ab"));
            }

            @Test
            void nested_word_token() {
                assertGetToken("a.b", wordToken("a.b"));
            }
        }

        @Nested
        class ItemToken {

            @Test
            void get_item_by_index() {
                assertGetToken("[1]", itemToken("[1]"));
            }

            @Test
            void item_token_end_char() {
                assertGetToken("[1][2]", itemToken("[1]"), itemToken("[2]"));
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

        @Nested
        class OperatorToken {
            @Test
            void supported_single_char_operators() {
                assertGetToken("=", operatorToken("="));
                assertGetToken(">", operatorToken(">"));
                assertGetToken("<", operatorToken("<"));
                assertGetToken("-", operatorToken("-"));
                assertGetToken("+", operatorToken("+"));
                assertGetToken("*", operatorToken("*"));
                assertGetToken("/", operatorToken("/"));
            }

            @Test
            void supported_two_chars_operators() {
                assertGetToken(">=", operatorToken(">="));
                assertGetToken("<=", operatorToken("<="));
                assertGetToken("&&", operatorToken("&&"));
                assertGetToken("||", operatorToken("||"));
            }
        }

        @Nested
        class SplitTokens {

            @Test
            void split_number() {
                assertGetToken("1 ", numebrToken(new BigDecimal(1)));
                assertGetToken("1\t", numebrToken(new BigDecimal(1)));
                assertGetToken("1\n", numebrToken(new BigDecimal(1)));
                assertGetToken("1=", numebrToken(new BigDecimal(1)), operatorToken("="));
                assertGetToken("1>", numebrToken(new BigDecimal(1)), operatorToken(">"));
                assertGetToken("1<", numebrToken(new BigDecimal(1)), operatorToken("<"));
                assertGetToken("1-", numebrToken(new BigDecimal(1)), operatorToken("-"));
                assertGetToken("1+", numebrToken(new BigDecimal(1)), operatorToken("+"));
                assertGetToken("1*", numebrToken(new BigDecimal(1)), operatorToken("*"));
                assertGetToken("1/", numebrToken(new BigDecimal(1)), operatorToken("/"));
                assertGetToken("1|", numebrToken(new BigDecimal(1)), operatorToken("|"));
                assertGetToken("1&", numebrToken(new BigDecimal(1)), operatorToken("&"));
                assertGetToken("1||", numebrToken(new BigDecimal(1)), operatorToken("||"));
                assertGetToken("1&&", numebrToken(new BigDecimal(1)), operatorToken("&&"));
            }

            @Test
            void split_word_token() {
                assertGetToken(".a ", propertyToken(".a"));
                assertGetToken(".a[1]", propertyToken(".a"), itemToken("[1]"));
                assertGetToken(".a=", propertyToken(".a"), operatorToken("="));
            }
        }
    }
}