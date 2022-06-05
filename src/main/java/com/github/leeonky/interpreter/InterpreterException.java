package com.github.leeonky.interpreter;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import static com.github.leeonky.interpreter.FunctionUtil.notAllowParallelReduce;
import static java.util.Collections.nCopies;

public class InterpreterException extends RuntimeException {
    private final List<Position> positions = new ArrayList<>();

    public InterpreterException(String message, int position) {
        this(message, position, Position.Type.CHAR);
    }

    public InterpreterException(String message, int position, Position.Type type) {
        super(message);
        positions.add(new Position(type, position));
    }

    @SuppressWarnings("unchecked")
    public <E extends InterpreterException> E multiPosition(int positionBegin, Position.Type type) {
        positions.add(new Position(type, positionBegin));
        return (E) this;
    }

    public String show(String code) {
        return show(code, 0);
    }

    public String show(String code, int offset) {
        positions.sort(Comparator.<Position>comparingInt(o -> o.position).reversed()
                .thenComparing(Comparator.<Position, Position.Type>comparing(o -> o.type).reversed()));
        return positions.stream().map(position -> position.offset(offset))
                .reduce(code.substring(offset), (r, p) -> p.process(r), notAllowParallelReduce());
    }

    public void setType(Position.Type type) {
        if (positions.size() > 0)
            positions.set(0, new Position(type, positions.get(0).position));
    }

    public void clearPosition() {
        positions.clear();
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
            return code.substring(0, endLineIndex) + "\n"
                    + type.markLine(code, position, code.lastIndexOf('\n', position), endLineIndex)
                    + code.substring(endLineIndex);
        }

        public Position offset(int offset) {
            return offset == 0 ? this : new Position(type, position - offset);
        }

        public enum Type {
            CHAR {
                @Override
                protected String markLine(String code, int position, int startLineIndex, int endLineIndex) {
                    return String.join("", nCopies(charCountBeforeMark(code, position, startLineIndex), " ")) + "^";
                }
            },
            LINE {
                @Override
                protected String markLine(String code, int position, int startLineIndex, int endLineIndex) {
                    return String.join("", nCopies(charCountBeforeMark(code, endLineIndex, startLineIndex), "^"));
                }
            };

            private static int charCountBeforeMark(String code, int position, int startLineIndex) {
                int fullWidthCharCount = (int) code.chars().limit(position)
                        .skip(startLineIndex == -1 ? 0 : startLineIndex).filter(Type::isFullWidth).count();
                return position - startLineIndex + fullWidthCharCount - 1;
            }

            private static boolean isFullWidth(int c) {
                return !('\u0000' <= c && c <= '\u00FF' || '\uFF61' <= c && c <= '\uFFDC' || '\uFFE8' <= c && c <= '\uFFEE');
            }

            protected abstract String markLine(String code, int position, int startOfCurrentLine, int endLineIndex);
        }
    }
}
