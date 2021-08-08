package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.token.SourceCode;

import java.util.function.Consumer;

public abstract class ContentPreprocessor<ContentType, P extends ContentPreprocessor<ContentType, P>> {
    private Consumer<SourceCode> preprocessor = sourceCode -> {
    };

    //TODO not call super
    public static <P extends ContentPreprocessor<?, P>> P leftTrim(P getter) {
        return getter.copy().setPreprocessor(SourceCode::trimLeft);
    }

    @SuppressWarnings("unchecked")
    protected P setPreprocessor(Consumer<SourceCode> preprocessor) {
        this.preprocessor = preprocessor;
        return (P) this;
    }

    void preprocess(SourceCode sourceCode) {
        preprocessor.accept(sourceCode);
    }

    protected P copy() {
        return newInstance().setPreprocessor(preprocessor);
    }

    protected abstract P newInstance();
}
