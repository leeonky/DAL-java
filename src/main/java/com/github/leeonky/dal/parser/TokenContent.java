package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.token.SourceCode;

import java.util.function.Consumer;
import java.util.function.Function;

public class TokenContent {

    public static final TokenContent ALL_CHARACTERS = new TokenContent();
    private Consumer<SourceCode> preprocessor;
    private Function<SourceCode, Character> charGetter;

    private TokenContent() {
        preprocessor = sourceCode -> {
        };
        charGetter = SourceCode::takeCurrentChar;
    }

    //TODO not call super
    public static TokenContent leftTrim(TokenContent getter) {
        TokenContent copy = getter.copy();
        copy.preprocessor = SourceCode::trimLeft;
        return copy;
    }

    void preprocess(SourceCode sourceCode) {
        preprocessor.accept(sourceCode);
    }

    char getChar(SourceCode sourceCode) {
        return charGetter.apply(sourceCode);
    }

    public TokenContent escape(String replace, char c) {
        TokenContent copy = copy();
        copy.charGetter = sourceCode -> sourceCode.startsWithThenSeek(replace) ? c : getChar(sourceCode);
        return copy;
    }

    public TokenContent copy() {
        TokenContent tokenContent = new TokenContent();
        tokenContent.charGetter = charGetter;
        tokenContent.preprocessor = preprocessor;
        return tokenContent;
    }
}
