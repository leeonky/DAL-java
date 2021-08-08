package com.github.leeonky.dal;

import com.github.leeonky.dal.token.Scanner;
import com.github.leeonky.dal.token.SourceCode;
import com.github.leeonky.dal.token.Token;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static com.github.leeonky.dal.token.Token.*;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

//TODO clean some duplicated UT
class ScannerTest {

    @Test
    void empty_source_code_should_return_empty_list() {
        assertEmptySourceCode("");
        assertEmptySourceCode("  ");
    }

    private void assertEmptySourceCode(String sourceCode) {
        assertThat(new Scanner().scan(new SourceCode(sourceCode)).allTokens()).isEmpty();
    }

    private void assertScanTokens(String sourceCode, Token... tokens) {
        assertThat(new Scanner().scan(new SourceCode(sourceCode)).allTokens()).containsOnly(tokens);
    }

    private void assertCompileError(String sourceCode, int position, String message) {
        SyntaxException syntaxException = assertThrows(SyntaxException.class, () -> new Scanner().scan(new SourceCode(sourceCode)));
        assertThat(syntaxException).hasMessage(message).hasFieldOrPropertyWithValue("position", position);
    }

    @Nested
    class NumberToken {

        @Test
        void should_support_number_and_convert_big_decimal() {
            assertScanTokens("1", constValueToken(new BigDecimal(1)));
            assertScanTokens("11", constValueToken(new BigDecimal(11)));
        }
    }

    @Nested
    class WordToken {

        @Test
        void single_word_token() {
            assertScanTokens("ab", wordToken("ab"));
        }

        @Test
        void should_not_split_word_for_char_point() {
            assertScanTokens("a.b", wordToken("a.b"));
        }
    }

    @Nested
    class PropertyToken {

        @Test
        void should_support_single_property() {
            assertScanTokens(".a", propertyToken("a"));
        }

        @Test
        void should_support_const_integer_index() {
            assertScanTokens("[1]", propertyToken(1));
        }
    }

    @Nested
    class OperatorToken {

        @Test
        void supported_single_char_operators() {
            assertScanTokens("=", operatorToken("="));
            assertScanTokens(">", operatorToken(">"));
            assertScanTokens("<", operatorToken("<"));
            assertScanTokens("-", operatorToken("-"));
            assertScanTokens("+", operatorToken("+"));
            assertScanTokens("*", operatorToken("*"));
            assertScanTokens("/", operatorToken("/"));
        }

        @Test
        void supported_double_char_operators() {
            assertScanTokens(">=", operatorToken(">="));
            assertScanTokens("<=", operatorToken("<="));
            assertScanTokens("&&", operatorToken("&&"));
            assertScanTokens("||", operatorToken("||"));
            assertScanTokens("!=", operatorToken("!="));
        }
    }

    @Nested
    class BracketToken {

        @Test
        void begin_end_bracket_token() {
            assertScanTokens("(", beginBracketToken());
            assertScanTokens(")", endBracketToken());
            assertScanTokens("(0)", beginBracketToken(), constValueToken(new BigDecimal(0)), endBracketToken());
        }
    }

    @Nested
    class SingleQuotationStringToken {

        @Test
        void should_ignore_blank_around_string() {
            assertScanTokens(" 'a' 'b'", constValueToken("a"), constValueToken("b"));
        }
    }

    @Nested
    class DoubleQuotationStringToken {

        @Test
        void should_ignore_blank_around_string() {
            assertScanTokens(" \"a\" \"b\"", constValueToken("a"), constValueToken("b"));
        }
    }

    @Nested
    class SplitTokens {

        @Test
        void split_number() {
            assertScanTokens("11 12", constValueToken(new BigDecimal(11)), constValueToken(new BigDecimal(12)));
            assertScanTokens("11\t", constValueToken(new BigDecimal(11)));
            assertScanTokens("11\n", constValueToken(new BigDecimal(11)));

            assertScanTokens("11=", constValueToken(new BigDecimal(11)), operatorToken("="));
            assertScanTokens("11>", constValueToken(new BigDecimal(11)), operatorToken(">"));
            assertScanTokens("11<", constValueToken(new BigDecimal(11)), operatorToken("<"));
            assertScanTokens("11-", constValueToken(new BigDecimal(11)), operatorToken("-"));
            assertScanTokens("11+", constValueToken(new BigDecimal(11)), operatorToken("+"));
            assertScanTokens("11*", constValueToken(new BigDecimal(11)), operatorToken("*"));
            assertScanTokens("11/", constValueToken(new BigDecimal(11)), operatorToken("/"));
            assertScanTokens("11|", constValueToken(new BigDecimal(11)), operatorToken("|"));
            assertScanTokens("11&", constValueToken(new BigDecimal(11)), operatorToken("&"));
            assertScanTokens("11||", constValueToken(new BigDecimal(11)), operatorToken("||"));
            assertScanTokens("11&&", constValueToken(new BigDecimal(11)), operatorToken("&&"));

            assertScanTokens("11(", constValueToken(new BigDecimal(11)), beginBracketToken());
            assertScanTokens("11)", constValueToken(new BigDecimal(11)), endBracketToken());

            assertScanTokens("11[1]", constValueToken(new BigDecimal(11)), propertyToken(1));
        }

        @Test
        void split_word_token() {
            assertScanTokens("a b", wordToken("a"), wordToken("b"));

            assertScanTokens("a=", wordToken("a"), operatorToken("="));

            assertScanTokens("a[0]", wordToken("a"), propertyToken(0));

            assertScanTokens("a(", wordToken("a"), beginBracketToken());
            assertScanTokens("a)", wordToken("a"), endBracketToken());
        }

        @Test
        void split_property_token() {
            assertScanTokens(".a .b", propertyToken("a"), propertyToken("b"));

            assertScanTokens(".a[1]", propertyToken("a"), propertyToken(1));

            assertScanTokens(".a=", propertyToken("a"), operatorToken("="));

            assertScanTokens(".a(", propertyToken("a"), beginBracketToken());
            assertScanTokens(".a)", propertyToken("a"), endBracketToken());
        }

        @Test
        void split_operator_token() {
            assertScanTokens("= =", operatorToken("="), operatorToken("="));

            assertScanTokens("==", operatorToken("=="));

            assertScanTokens("=1", operatorToken("="), constValueToken(new BigDecimal(1)));

            assertScanTokens("=a", operatorToken("="), wordToken("a"));

            assertScanTokens("=(", operatorToken("="), beginBracketToken());
            assertScanTokens("=)", operatorToken("="), endBracketToken());

            assertScanTokens("=.a", operatorToken("="), propertyToken("a"));

            assertScanTokens("=[0]", operatorToken("="), propertyToken(0));

            assertScanTokens("='0'", operatorToken("="), constValueToken("0"));

            assertScanTokens("=\"0\"", operatorToken("="), constValueToken("0"));
        }
    }
}