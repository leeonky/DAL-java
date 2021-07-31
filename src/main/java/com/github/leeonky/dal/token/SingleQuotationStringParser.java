package com.github.leeonky.dal.token;

class SingleQuotationStringParser {
    private final StringBuffer stringBuffer = new StringBuffer();
    private boolean isEscape = false;
    private int startEndCount = 0;

    public boolean feed(char c) {
        if (isFinished())
            throw new IllegalArgumentException("string is complete!");
        if (isEscape) {
            if (c != '\'' && c != '\\')
                stringBuffer.append('\\');
            stringBuffer.append(c);
            isEscape = false;
        } else {
            if (c == '\\')
                isEscape = true;
            else {
                if (c == '\'')
                    startEndCount++;
                stringBuffer.append(c);
            }
        }
        return startEndCount < 2;
    }

    public boolean isFinished() {
        return startEndCount == 2;
    }

    public String value() {
        if (!isFinished())
            throw new IllegalStateException(String.format("%s not finished", stringBuffer.toString()));
        return stringBuffer.subSequence(1, stringBuffer.length() - 1).toString();
    }
}
