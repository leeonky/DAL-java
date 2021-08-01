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
            append(escape(c));
        else if (isEscapeChar(c))
            isEscape = true;
        else if (!trimFirstCharOnceIfNeeded() && !(isFinished = isFinishedChar(c)))
            append(c);
        return !isFinished();
    }

    protected void append(char c) {
        content.append(c);
    }

    protected void append(String escape) {
        content.append(escape);
    }

    private boolean toggleEscape() {
        return isEscape && !(isEscape = false);
    }

    private boolean trimFirstCharOnceIfNeeded() {
        return isEmpty() && !isFirstCharSkipped && trimFirstChar() && (isFirstCharSkipped = true);
    }

    protected boolean isEmpty() {
        return content.length() == 0;
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
        return content();
    }

    protected String content() {
        return content.toString();
    }
}
