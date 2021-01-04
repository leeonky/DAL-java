package com.github.leeonky.dal;

import java.io.BufferedReader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static java.util.stream.Collectors.toCollection;

public class DalException extends java.lang.RuntimeException {
    private final int position;

    protected DalException(String message, int position) {
        super(message);
        this.position = position;
    }

    public int getPosition() {
        return position;
    }

    public String show(String code) {
        try {
            List<String> codeLines = new BufferedReader(new StringReader(code)).lines()
                    .collect(toCollection(ArrayList::new));
            int position = this.position;
            int lineIndex = 0;
            for (; lineIndex < codeLines.size() && position > codeLines.get(lineIndex).length(); lineIndex++)
                position -= codeLines.get(lineIndex).length() + 1;
            codeLines.add(lineIndex + 1, String.join("", Collections.nCopies(position, " ")) + "^");
            return String.join("\n", codeLines);
        } catch (Exception exception) {
            throw new IllegalStateException(exception);
        }
    }
}
