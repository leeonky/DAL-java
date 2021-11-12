package com.github.leeonky.dal.runtime;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import static java.util.Collections.nCopies;

public class DalException extends java.lang.RuntimeException {
    private final List<Position> positions = new ArrayList<>();

    protected DalException(String message, int position) {
        this(message, position, Position.Type.CHAR);
    }

    protected DalException(String message, int position, Position.Type type) {
        super(message);
        positions.add(new Position(type, position));
    }

    public DalException multiPosition(int positionBegin, Position.Type type) {
        positions.add(new Position(type, positionBegin));
        return this;
    }

    @Deprecated
    public int getPosition() {
        return positions.get(0).position;
    }

    public String show(String code) {
        String result = code;
        positions.sort(Comparator.<Position>comparingInt(o -> o.position).reversed()
                .thenComparing(Comparator.<Position, Position.Type>comparing(o -> o.type).reversed()));
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
