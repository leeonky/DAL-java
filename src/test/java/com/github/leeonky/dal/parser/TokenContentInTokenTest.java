package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.token.SourceCode;
import com.github.leeonky.dal.token.Token;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static com.github.leeonky.dal.parser.TokenContentInToken.byFactory;
import static com.github.leeonky.dal.token.TokenFactory.createNumberTokenFactory;
import static com.github.leeonky.dal.token.TokenFactory.createSingleQuotedStringTokenFactory;
import static org.assertj.core.api.Assertions.assertThat;

class TokenContentInTokenTest {

    @Nested
    class ByFactory {
        private Token parseToken(String sourceCode, TokenContentInToken content) {
            return content.getToken(new TokenParser(new SourceCode(sourceCode)));
        }

        @Test
        void can_parse_sub_token_by_given_factory() {
            assertThat(parseToken("'1'", byFactory(createSingleQuotedStringTokenFactory())))
                    .isEqualTo(Token.constValueToken("1"));
        }

        @Test
        void can_parse_sub_token_by_given_factories() {
            assertThat(parseToken("100", byFactory(createSingleQuotedStringTokenFactory())
                    .or(createNumberTokenFactory())))
                    .isEqualTo(Token.constValueToken(new BigDecimal("100")));
        }
    }
}