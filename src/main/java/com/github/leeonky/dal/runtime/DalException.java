package com.github.leeonky.dal.runtime;

import java.util.Comparator;
import java.util.TreeSet;

import static java.util.Collections.nCopies;

public class DalException extends java.lang.RuntimeException {
    private final TreeSet<Position> positions = new TreeSet<>(Comparator.<Position>comparingInt(o -> o.position).reversed());

    protected DalException(String message, int position) {
        super(message);
        positions.add(new Position(Position.Type.CHAR, position));
    }

    public DalException multiPosition(int positionBegin, Position.Type type) {
        positions.add(new Position(type, positionBegin));
        return this;
    }

    @Deprecated
    public int getPosition() {
        return positions.first().position;
    }

    public String show(String code) {
        String result = code;
        for (Position marker : positions)
            result = marker.process(result);
        return result;
    }

    public static class Position {
        private final Type type;
        private final int position;

        public Position(Type type, int position) {
            this.type = type;
            this.position = position;
        }

        public String process(String code) {
            int endLineIndex = code.indexOf('\n', position);
            endLineIndex = endLineIndex == -1 ? code.length() : endLineIndex;
            return code.substring(0, endLineIndex) + "\n" + type.markLine(code, position, endLineIndex)
                    + code.substring(endLineIndex);
        }

        public enum Type {
            CHAR {
                @Override
                protected String markLine(String code, int position, int endLineIndex) {
                    return String.join("", nCopies(position - code.lastIndexOf('\n', position) - 1, " ")) + "^";
                }
            },
            LINE {
                @Override
                protected String markLine(String code, int position, int endLineIndex) {
                    return String.join("", nCopies(endLineIndex - code.lastIndexOf('\n', position) - 1, "^"));
                }
            };

            protected abstract String markLine(String code, int position, int endLineIndex);
        }
    }
}
