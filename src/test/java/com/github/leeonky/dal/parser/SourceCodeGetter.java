package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.token.SourceCode;

public abstract class SourceCodeGetter {
    public static final SourceCodeGetter DEFAULT = new SourceCodeGetter() {
        @Override
        public char getChar(SourceCode sourceCode) {
            return sourceCode.takeCurrentChar();
        }
    };

    abstract char getChar(SourceCode sourceCode);

    public SourceCodeGetter escape(String replace, char c) {
        return new SourceCodeGetter() {
            @Override
            public char getChar(SourceCode sourceCode) {
                return sourceCode.startsWithThenSeek(replace) ? c : SourceCodeGetter.this.getChar(sourceCode);
            }
        };
    }
}
