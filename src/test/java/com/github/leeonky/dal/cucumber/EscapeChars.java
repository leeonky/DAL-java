package com.github.leeonky.dal.cucumber;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;

//TODO move to Source Code Inner
public class EscapeChars {
    private final List<EscapeChar> escapeChars = new ArrayList<>();

    public EscapeChars escape(String target, char c) {
        escapeChars.add(new EscapeChar(target, c));
        return this;
    }

    public Optional<Character> escapeAt(String code, int position, Consumer<Integer> codeLength) {
        return escapeChars.stream().filter(e -> e.matchesAt(code, position))
                .findFirst().map(escapeChar -> escapeChar.returnTargetLengthAndPopEscapedChar(codeLength));
    }

    private static class EscapeChar {
        private final String target;
        private final char c;

        EscapeChar(String target, char c) {
            this.target = target;
            this.c = c;
        }

        boolean matchesAt(String code, int startIndex) {
            return code.startsWith(target, startIndex);
        }

        private char returnTargetLengthAndPopEscapedChar(Consumer<Integer> escapedLength) {
            escapedLength.accept(target.length());
            return c;
        }
    }
}
