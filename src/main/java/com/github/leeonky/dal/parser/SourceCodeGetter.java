package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.token.SourceCode;

import java.util.function.Consumer;
import java.util.function.Function;

//TODO rename
public class SourceCodeGetter {

    public static final SourceCodeGetter ALL_CHARACTERS = new SourceCodeGetter();
    private Consumer<SourceCode> preprocessor;
    //TODO rename
    private Function<SourceCode, Character> charGetter;

    private SourceCodeGetter() {
        preprocessor = sourceCode -> {
        };
        charGetter = SourceCode::takeCurrentChar;
    }

    //TODO call super
    //TODO extract method to support call super preprocessor
    public static SourceCodeGetter leftTrim(SourceCodeGetter getter) {
        SourceCodeGetter copy = getter.copy();
        copy.preprocessor = SourceCode::trimLeft;
        return copy;
    }

    void preprocess(SourceCode sourceCode) {
        preprocessor.accept(sourceCode);
    }

    char getChar(SourceCode sourceCode) {
        return charGetter.apply(sourceCode);
    }

    //TODO extract method to support call super getChar
    public SourceCodeGetter escape(String replace, char c) {
        SourceCodeGetter copy = copy();
        copy.charGetter = sourceCode -> sourceCode.startsWithThenSeek(replace) ? c : getChar(sourceCode);
        return copy;
    }

    public SourceCodeGetter copy() {
        SourceCodeGetter sourceCodeGetter = new SourceCodeGetter();
        sourceCodeGetter.charGetter = charGetter;
        sourceCodeGetter.preprocessor = preprocessor;
        return sourceCodeGetter;
    }
}
