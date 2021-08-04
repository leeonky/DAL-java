package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntaxException;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

interface StartWith2 {
    static StartWith2 excluded(SourceCodeMatches sourceCodeMatcher) {
        return sourceCodeMatcher::excluded;
    }

    boolean matches(ParseContext context);
}

interface EndWith2 {
    static EndWith2 excluded(SourceCodeMatches sourceCodeMatcher) {
        return sourceCodeMatcher::excluded;
    }

    boolean matches(ParseContext context);

    default EndWith2 orThrow(String errorMessage) {
        return context -> {
            try {
                return matches(context);
            } catch (NoMoreSourceCodeException ignore) {
                throw new SyntaxException(context.sourceCode.getPosition(), errorMessage);
            }
        };
    }
}

interface SourceCodeMatches {
    static SourceCodeMatches CHARACTER(char c) {
        return context -> {
            if (context.sourceCode.currentChar() == c)
                return 1;
            return 0;
        };
    }

    default boolean excluded(ParseContext context) {
        int count = matches(context);
        context.sourceCode.seek(count);
        return count > 0;
    }

    int matches(ParseContext context);
}

interface Content {
    static Content allChars() {
        return context -> context.sourceCode.takeCurrentChar();
    }

    Character getChar(ParseContext context);

    default Content escape(String escape, char c) {
        return context -> {
            if (context.sourceCode.startsWith(escape)) {
                context.sourceCode.seek(escape.length());
                return c;
            }
            return getChar(context);
        };
    }
}

abstract class TokenFactoryBase2 implements TokenFactory {
    private final StartWith2 startWith;
    private final EndWith2 endWith;
    private final Content content;

    protected TokenFactoryBase2(StartWith2 startWith, Content content, EndWith2 endWith) {
        this.startWith = startWith;
        this.endWith = endWith;
        this.content = content;
    }

    @Override
    public Token fetchToken(SourceCode sourceCode, Token previous) {
        ParseContext context = new ParseContext(sourceCode, previous);
        if (startWith.matches(context)) {
            while (!endWith.matches(context))
                context.content.add(content.getChar(context));
            return createToken(context.content.stream().map(Objects::toString).collect(Collectors.joining()));
        }
        return null;
    }

    protected abstract Token createToken(String content);

}

class ParseContext {
    //TODO to be private
    final SourceCode sourceCode;
    final Token last;
    final List<Character> content = new ArrayList<>();

    ParseContext(SourceCode sourceCode, Token last) {
        this.sourceCode = sourceCode;
        this.last = last;
    }
}