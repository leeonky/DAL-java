package com.github.leeonky.dal.token;

public abstract class TokenParser {
    private final StringBuffer content = new StringBuffer();
    private boolean isEscape = false;
    private boolean isFinished = false;
    private boolean isFirstCharSkipped = false;

    protected abstract String escape(char c);

    public boolean feed(char c) {
        if (isFinished())
            throw new IllegalArgumentException("content is finished!");
        if (toggleEscape())
            content.append(escape(c));
        else if (isEscapeChar(c))
            isEscape = true;
        else if (!trimFirstCharOnceIfNeeded() && !(isFinished = isFinishedChar(c)))
            content.append(c);
        return !isFinished();
    }

    private boolean toggleEscape() {
        return isEscape && !(isEscape = false);
    }

    private boolean trimFirstCharOnceIfNeeded() {
        return content.length() == 0 && !isFirstCharSkipped && trimFirstChar() && (isFirstCharSkipped = true);
    }

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
        return content.toString();
    }
}
