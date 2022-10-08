package com.github.leeonky.dal.util;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import static java.util.Arrays.asList;
import static java.util.stream.Collectors.joining;

public class TextUtil {
    public static final Set<String> SPLITTERS = new LinkedHashSet<>(asList("\r\n", "\n\r", "\n", "\r"));

    private static List<String> lines(String content, List<String> list) {
        for (String str : SPLITTERS) {
            int index = content.indexOf(str);
            if (index != -1) {
                lines(content.substring(0, index), list);
                return lines(content.substring(index + str.length()), list);
            }
        }
        list.add(content);
        return list;
    }

    public static List<String> lines(String content) {
        return lines(content, new ArrayList<>());
    }

    public static String join(List<Character> characters) {
        StringBuilder builder = new StringBuilder();
        characters.forEach(builder::append);
        return builder.toString();
    }

    public static String indent(String content) {
        return lines(content).stream().map(l -> "    " + l).collect(joining("\n"));
    }
}
