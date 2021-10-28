package com.github.leeonky.dal.runtime;

import static java.util.Collections.nCopies;

public class DalException extends java.lang.RuntimeException {
    private final int position;
    private Position secondPosition;

    protected DalException(String message, int position) {
        super(message);
        this.position = position;
    }

    public int getPosition() {
        return position;
    }

    public DalException multiPosition(int positionBegin) {
        secondPosition = new Position(Position.Type.CHAR, positionBegin);
        return this;
    }

    public String show(String code) {
        int line = code.indexOf('\n', position);
        String firstPart = firstPart(code, line);
        return firstPart + "\n" + String.join("", nCopies(position - firstPart.lastIndexOf('\n') - 1, " ")) + "^"
                + lastPart(code, line);
    }

    private String lastPart(String str, int begin) {
        return begin == -1 ? "" : str.substring(begin);
    }

    private String firstPart(String str, int end) {
        return end == -1 ? str : str.substring(0, end);
    }

    public static class Position {
        private final Type type;
        private final int position;

        public Position(Type type, int position) {
            this.type = type;
            this.position = position;
        }

        public enum Type {
            CHAR,
            LINE,
        }
    }
}
