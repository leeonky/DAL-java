package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.token.SourceCode;

import java.util.function.Function;

public class TokenContentInString extends ContentPreprocessor<String, TokenContentInString> {
    public static final TokenContentInString ALL_CHARACTERS = new TokenContentInString();
    private Function<SourceCode, Character> charGetter = SourceCode::takeCurrentChar;

    private TokenContentInString() {
        super();
    }

    protected TokenContentInString setCharGetter(Function<SourceCode, Character> charGetter) {
        this.charGetter = charGetter;
        return this;
    }

    char getChar(SourceCode sourceCode) {
        return charGetter.apply(sourceCode);
    }

    public TokenContentInString escape(String replace, char c) {
        return copy().setCharGetter(sourceCode -> sourceCode.startsWithThenSeek(replace) ? c : getChar(sourceCode));
    }

    @Override
    public TokenContentInString copy() {
        return super.copy().setCharGetter(charGetter);
    }

    @Override
    protected TokenContentInString newInstance() {
        return new TokenContentInString();
    }
}
