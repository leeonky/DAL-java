package com.github.leeonky.dal.runtime;

import com.ibm.icu.lang.UCharacter;
import com.ibm.icu.lang.UProperty;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

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
        return show(code, 0);
    }

    public String show(String code, int offset) {
        String result = code.substring(offset);
        positions.sort(Comparator.<Position>comparingInt(o -> o.position).reversed()
                .thenComparing(Comparator.<Position, Position.Type>comparing(o -> o.type).reversed()));
        for (Position marker : positions.stream().map(position -> position.offset(offset)).collect(Collectors.toList()))
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

        public Position offset(int offset) {
            return offset == 0 ? this : new Position(type, position - offset);
        }

        public enum Type {
            CHAR {
                @Override
                protected String markLine(String code, int position, int endLineIndex) {
                    int startOfCurrentLine = code.lastIndexOf('\n', position);
                    int fullWidthCharCount = (int) code.chars().limit(position).skip(startOfCurrentLine == -1 ? 0 : startOfCurrentLine)
                            .filter(Type::isFullWidthChar).count();

                    return String.join("", nCopies(position - startOfCurrentLine + fullWidthCharCount - 1, " ")) + "^";
                }
            },
            LINE {
                @Override
                protected String markLine(String code, int position, int endLineIndex) {
                    int startOfCurrentLine = code.lastIndexOf('\n', position);
                    int fullWidthCharCount = (int) code.chars().limit(endLineIndex).skip(startOfCurrentLine == -1 ? 0 : startOfCurrentLine)
                            .filter(Type::isFullWidthChar).count();
                    return String.join("", nCopies(endLineIndex - startOfCurrentLine - 1 + fullWidthCharCount, "^"));
                }
            };

            private static boolean isFullWidthChar(int c) {
                return (UCharacter.getIntPropertyValue(c, UProperty.EAST_ASIAN_WIDTH) & UCharacter.EastAsianWidth.FULLWIDTH) != 0;
            }

            protected abstract String markLine(String code, int position, int endLineIndex);
        }
    }
}
