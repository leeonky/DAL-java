package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.Constants;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static java.util.stream.Collectors.joining;

public class ParsedCode {
    private final List<Character> content = new ArrayList<>();

    public boolean isOperatorMatches() {
        return content.size() == 1 && Constants.OPT_MATCHES == content.get(0);
    }

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
        //TODO need test
//        parsedCode.content.clear();
    }
}