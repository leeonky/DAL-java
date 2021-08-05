package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntaxException;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

interface StartWith {
    static StartWith excluded(ContextMatches contextMatches) {
        return contextMatches::excluded;
    }

    static StartWith included(ContextMatches contextMatches) {
        return contextMatches::included;
    }

    boolean matches(ParseContext context);
}

interface EndWith {
    EndWith NO_MORE_SOURCE_CODE = context -> !context.sourceCode.notEnd();

    static EndWith excluded(ContextMatches sourceCodeMatcher) {
        return sourceCodeMatcher::excluded;
    }

    static EndWith before(ContextMatches sourceCodeMatcher) {
        return sourceCodeMatcher::before;
    }

    boolean matches(ParseContext context);

    default EndWith orThrow(String errorMessage) {
        return context -> {
            try {
                return matches(context);
            } catch (NoMoreSourceCodeException ignore) {
                throw new SyntaxException(context.sourceCode.getPosition(), errorMessage);
            }
        };
    }

    default EndWith or(EndWith another) {
        return context -> matches(context) || another.matches(context);
    }
}

interface ContextPredicate {
    ContextPredicate AFTER_TOKEN_MATCHES = context -> context.last != null && context.last.isOperatorMatches();
    ContextPredicate AFTER_OPERATOR_MATCHES = context -> context.content.size() == 1 && context.content.get(0) == '~';

    boolean test(ParseContext context);
}

interface ContextMatches {
    ContextMatches DIGITAL = context -> {
        if (Character.isDigit(context.sourceCode.currentChar()))
            return 1;
        return -1;
    };

    ContextMatches TOKEN_DELIMITER = context -> {
        if (Scanner.TOKEN_DELIMITER.contains(context.sourceCode.currentChar()))
            return 1;
        return -1;
    };

    ContextMatches OPERATOR = context -> {
        if (Scanner.OPERATOR_CHAR.contains(context.sourceCode.currentChar()))
            return 1;
        return -1;
    };

    static ContextMatches CHARACTER(char c) {
        return context -> {
            if (context.sourceCode.currentChar() == c)
                return 1;
            return -1;
        };
    }

    static ContextMatches not(ContextMatches contextMatches) {
        return context -> -contextMatches.matches(context);
    }

    default boolean excluded(ParseContext context) {
        int count = matches(context);
        if (count > 0) {
            context.sourceCode.seek(count);
            return true;
        }
        return false;
    }

    default ContextMatches when(ContextPredicate predicate) {
        return context -> {
            int matches = matches(context);
            if (predicate.test(context))
                return matches;
            return -Math.abs(matches);
        };
    }

    int matches(ParseContext context);

    default boolean included(ParseContext context) {
        int count = matches(context);
        for (int i = 0; i < count; i++)
            context.content.add(context.sourceCode.takeCurrentChar());
        return count > 0;
    }

    default boolean before(ParseContext context) {
        return matches(context) > 0;
    }

    default ContextMatches except(ContextMatches exception) {
        return context -> {
            int matches = matches(context);
            if (matches > 0)
                if (exception.matches(context) > 0)
                    return -matches;
            return matches;
        };
    }
}

//TODO to refactor
interface Content {
    static Content allChars() {
        return context -> context.sourceCode.takeCurrentChar();
    }

    static Content leftTrim(Content content) {
        return new Content() {
            @Override
            public Character getChar(ParseContext context) {
                return content.getChar(context);
            }

            @Override
            public void cleanCode(SourceCode sourceCode) {
                sourceCode.trimLeft();
            }
        };
    }

    Character getChar(ParseContext context);

    default void cleanCode(SourceCode sourceCode) {
    }

    default Content escape(String escape, char c) {
        return new Content() {
            @Override
            public Character getChar(ParseContext context) {
                if (context.sourceCode.startsWith(escape)) {
                    context.sourceCode.seek(escape.length());
                    return c;
                }
                return Content.this.getChar(context);
            }

            @Override
            public void cleanCode(SourceCode sourceCode) {
                Content.this.cleanCode(sourceCode);
            }
        };
    }
}

abstract class TokenFactoryBase implements TokenFactory {
    //TODO rename
    private final StartWith startWith;

    //TODO rename
    private final EndWith endWith;
    private final Content content;

    //TODO rename
    protected TokenFactoryBase(StartWith startWith, Content content, EndWith endWith) {
        this.startWith = startWith;
        this.endWith = endWith;
        this.content = content;
    }

    @Override
    public Token fetchToken(SourceCode sourceCode, Token previous) {
        ParseContext context = new ParseContext(sourceCode, previous);
        if (startWith.matches(context)) {
            content.cleanCode(context.sourceCode);
            while (!endWith.matches(context))
                context.content.add(content.getChar(context));
            try {
                return createToken(context.content.stream().map(Objects::toString).collect(Collectors.joining()));
            } catch (IllegalTokenContentException e) {
                throw new SyntaxException(sourceCode.getPosition(), e.getMessage());
            }
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