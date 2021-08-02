package com.github.leeonky.dal.token;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public abstract class TokenFactoryBase implements TokenFactory {
    @Override
    public Token fetchToken(SourceCode sourceCode, Token previous) {
        if (sourceCode.notEnd() && isMatched(sourceCode)) {
            List<Character> content = new ArrayList<>();
            while (sourceCode.notEnd() && !isEnded(sourceCode.getChar()))
                content.add(sourceCode.takeChar());
            return createToken(content.stream().map(Objects::toString).collect(Collectors.joining()));
        }
        return null;
    }

    protected abstract boolean isEnded(char c);

    protected abstract boolean isMatched(SourceCode sourceCode);

    protected abstract Token createToken(String content);
}
