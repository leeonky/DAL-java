package com.github.leeonky.interpreter;

import java.util.Optional;

import static com.github.leeonky.util.function.When.when;

public class HotFix {
    public static String getCode(SourceCode sourceCode) {
        return sourceCode.charStream().getCode();
    }

    public static Optional<Token> popWord(String label, SourceCode sourceCode) {
        return when(sourceCode.startsWith(label)).optional(() -> {
            Token token = new Token(sourceCode.charStream().position()).append(label);
            sourceCode.charStream().seek(label.length());
            return token;
        });
    }
}
