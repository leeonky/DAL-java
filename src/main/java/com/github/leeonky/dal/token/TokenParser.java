package com.github.leeonky.dal.token;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public abstract class TokenParser {
    protected final List<Character> content = new ArrayList<>();
    private boolean isEscape = false;
    private boolean isFinished = false;
    private boolean isFirstCharSkipped = false;

    protected abstract String escape(char c);

    public boolean feed(char c) {
        if (isFinished())
            throw new IllegalArgumentException("content is finished!");
        if (toggleEscape())
            for (char c1 : escape(c).toCharArray())
                content.add(c1);
        else if (isEscapeChar(c))
            isEscape = true;
        else if (!trimFirstCharOnceIfNeeded() && !trimWhiteSpaceIfNeeded(c) && !(isFinished = isFinishedChar(c)))
            content.add(c);
        return !isFinished();
    }

    private boolean toggleEscape() {
        return isEscape && !(isEscape = false);
    }

    private boolean trimFirstCharOnceIfNeeded() {
        return content.size() == 0 && !isFirstCharSkipped && trimFirstChar() && (isFirstCharSkipped = true);
    }

    private boolean trimWhiteSpaceIfNeeded(char c) {
        return content.size() == 0 && trimWhiteSpaceChar() && Character.isWhitespace(c);
    }

    protected boolean trimWhiteSpaceChar() {
        return false;
    }

    //TODO to be refactored
    public boolean canFinish() {
        return isFinished();
    }

    protected abstract boolean isFinishedChar(char c);

    protected abstract boolean isEscapeChar(char c);

    public boolean isFinished() {
        return isFinished;
    }

    protected abstract boolean trimFirstChar();

    public String value() {
        if (!canFinish())
            throw new IllegalStateException("can not finish or content is finished");
        return content.stream().map(Object::toString).collect(Collectors.joining());
    }
}
