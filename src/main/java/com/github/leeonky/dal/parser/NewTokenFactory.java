package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.token.Token;
import com.github.leeonky.dal.token.TokenFactory;

import java.util.List;
import java.util.function.Function;

import static com.github.leeonky.dal.parser.ParsingContext.ANY_CHARACTERS;
import static com.github.leeonky.dal.parser.ParsingContext.CHARACTER;
import static com.github.leeonky.dal.parser.TokenContentInString.ALL_CHARACTERS;
import static com.github.leeonky.dal.parser.TokenStartEnd.before;

public class NewTokenFactory {

    private final TokenStartEnd start;

    private NewTokenFactory(TokenStartEnd start) {
        this.start = start;
    }

    public static NewTokenFactory startWith(TokenStartEnd startEnd) {
        return new NewTokenFactory(startEnd);
    }

    public static StringContent.EndWith equalToCharacter(char c) {
        return startWith(ParsingContext.included(CHARACTER(c))).take(ALL_CHARACTERS).endWith(ParsingContext.END_OF_CODE.or(before(ANY_CHARACTERS)));
    }

    public StringContent take(TokenContentInString content) {
        return new StringContent(content);
    }

    public StringContent.EndWith endWith(TokenStartEnd end) {
        return take(ALL_CHARACTERS).endWith(end);
    }

    public TokenContent take(TokenContentInToken content) {
        return new TokenContent(content);
    }

    public class StringContent {

        private final TokenContentInString content;

        public StringContent(TokenContentInString content) {
            this.content = content;
        }

        public EndWith endWith(TokenStartEnd end) {
            return new EndWith(end);
        }

        public class EndWith {
            private final TokenStartEnd end;

            public EndWith(TokenStartEnd end) {
                this.end = end;
            }

            public TokenFactory createAs(Function<String, Token> creator) {
                return context -> context.parseToken(start, content, end, creator);
            }
        }
    }

    public class TokenContent {
        private final TokenContentInToken content;

        public TokenContent(TokenContentInToken content) {
            this.content = content;
        }

        public EndWith endWith(TokenStartEnd end) {
            return new EndWith(end);
        }

        public class EndWith {
            private final TokenStartEnd end;

            public EndWith(TokenStartEnd end) {
                this.end = end;
            }

            public TokenFactory createAs(Function<List<Token>, Token> creator) {
                return context -> context.parseToken(start, content, end, creator);
            }
        }
    }
}
