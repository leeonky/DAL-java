package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.token.SourceCode;

import java.util.function.Consumer;
import java.util.function.Function;

public class TokenContentInString {
    public static final TokenContentInString ALL_CHARACTERS = new TokenContentInString();
    private Function<SourceCode, Character> charGetter = SourceCode::takeCurrentChar;
    private Consumer<SourceCode> preprocessor = sourceCode -> {
    };

    private TokenContentInString() {
    }

    public static TokenContentInString leftTrim(TokenContentInString getter) {
        return getter.copy().setPreprocessor(SourceCode::trimLeft);
    }

    public TokenContentInString escape(String replace, char c) {
        return copy().setCharGetter(sourceCode -> sourceCode.startsWithThenSeek(replace) ? c : getChar(sourceCode));
    }

    void preprocess(SourceCode sourceCode) {
        preprocessor.accept(sourceCode);
    }

    char getChar(SourceCode sourceCode) {
        return charGetter.apply(sourceCode);
    }

    protected TokenContentInString copy() {
        return new TokenContentInString().setCharGetter(charGetter).setPreprocessor(preprocessor);
    }

    private TokenContentInString setCharGetter(Function<SourceCode, Character> charGetter) {
        this.charGetter = charGetter;
        return this;
    }

    private TokenContentInString setPreprocessor(Consumer<SourceCode> preprocessor) {
        this.preprocessor = preprocessor;
        return this;
    }
}
