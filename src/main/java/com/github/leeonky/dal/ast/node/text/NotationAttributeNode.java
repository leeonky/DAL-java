package com.github.leeonky.dal.ast.node.text;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.dal.util.TextUtil;

import java.util.ArrayList;
import java.util.List;

import static com.github.leeonky.util.function.Extension.not;
import static java.util.stream.Collectors.toList;

public class NotationAttributeNode extends DALNode {
    private final TextBlockAttributeListNode attributeList;
    private final DALNode notation;

    public NotationAttributeNode(DALNode notation, TextBlockAttributeListNode attributeList) {
        this.notation = notation;
        this.attributeList = attributeList;
    }

    @Override
    public String inspect() {
        return (notation.inspect() + " " + attributeList.inspect()).trim();
    }

    public String endNotation() {
        return notation.inspect();
    }

    public Object text(List<Character> content, RuntimeContextBuilder.DALRuntimeContext context) {
        return attributeList.getFormatter(context).format(resolveToText(content), context);
    }

    private String resolveToText(List<Character> content) {
        List<String> lines = joinToLines(content);
        int indent = resolveIndent(lines);
        List<String> collect = lines.stream().map(s -> s.equals("") ? s : processLine(s, indent)).collect(toList());
        return collect.size() == 1 && collect.get(0).equals("") ? "\n" : String.join("\n", collect);
    }

    private int resolveIndent(List<String> lines) {
        return lines.stream().filter(not(String::isEmpty)).mapToInt(s -> {
            for (int i = 0; i < s.length(); i++)
                if (s.charAt(i) != ' ')
                    return i;
            return 0;
        }).min().orElse(0);
    }

    private List<String> joinToLines(List<Character> content) {
        // append a blank char to keep tail \n to one line in lines method
        List<String> lines = TextUtil.lines(TextUtil.join(content) + ' ');
        return new ArrayList<>(lines.subList(0, lines.size() - 1));
    }

    private String processLine(String s, int indent) {
        return s.substring(indent);
    }
}
