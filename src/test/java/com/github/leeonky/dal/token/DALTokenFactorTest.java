package com.github.leeonky.dal.token;

import com.github.leeonky.dal.Constants;
import com.github.leeonky.dal.parser.TokenParser;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static com.github.leeonky.dal.token.Token.Type.TREE;
import static com.github.leeonky.dal.token.TokenFactory.createDALTokenFactory;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;

class DALTokenFactorTest {

    @Test
    void return_empty_when_empty_source() {
        Token token = parseToken("");

        assertThat(token.getType()).isEqualTo(TREE);
        assertFalse(token.getTokenStream().hasTokens());
    }

    @Test
    void return_empty_when_source_with_white_space() {
        Token token = parseToken(" ");

        assertThat(token.getType()).isEqualTo(TREE);
        assertFalse(token.getTokenStream().hasTokens());
    }

    @Test
    void could_parse_all_dal_tokens() {
        TokenStream tokenStream = parseToken("[0] .id 100 'str1'\"str2\" : /abc/ ( ) which").getTokenStream();
        assertThat(tokenStream.pop()).isEqualTo(Token.propertyToken(0));
        assertThat(tokenStream.pop()).isEqualTo(Token.propertyToken("id"));
        assertThat(tokenStream.pop()).isEqualTo(Token.constValueToken(BigDecimal.valueOf(100)));
        assertThat(tokenStream.pop()).isEqualTo(Token.constValueToken("str1"));
        assertThat(tokenStream.pop()).isEqualTo(Token.constValueToken("str2"));
        assertThat(tokenStream.pop()).isEqualTo(Token.operatorToken(":"));
        assertThat(tokenStream.pop()).isEqualTo(Token.regexToken("abc"));
        assertThat(tokenStream.pop()).isEqualTo(Token.beginBracketToken());
        assertThat(tokenStream.pop()).isEqualTo(Token.endBracketToken());
        assertThat(tokenStream.pop()).isEqualTo(Token.keyWordToken(Constants.KeyWords.WHICH));
    }

    private Token parseToken(String s) {
        return createDALTokenFactory().fetchToken(new TokenParser(new SourceCode(s)));
    }
}
