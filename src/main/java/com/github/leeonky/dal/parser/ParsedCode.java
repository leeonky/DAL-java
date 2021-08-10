package com.github.leeonky.dal.parser;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static java.util.stream.Collectors.joining;

public class ParsedCode {
    private final List<Character> content = new ArrayList<>();

    public String takeContent() {
        String collect = content.stream().map(Objects::toString).collect(joining());
        content.clear();
        return collect;
    }

    public void feed(char c) {
        content.add(c);
    }

    public void feed(ParsedCode parsedCode) {
        content.addAll(parsedCode.content);
        parsedCode.content.clear();
    }

    public boolean isSourceCode(String code) {
        return content.stream().map(Objects::toString).collect(joining()).equals(code);
    }
}