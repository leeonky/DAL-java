package com.github.leeonky.dal.ast.node.text;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.List;
import java.util.stream.Collectors;

import static com.github.leeonky.dal.util.TextUtil.lines;
import static java.util.Collections.nCopies;

public class NotationAttributeNode extends DALNode {
    private final TextAttributeListNode attributeList;
    private final DALNode notation;

    public NotationAttributeNode(DALNode notation, TextAttributeListNode attributeList) {
        this.notation = notation;
        this.attributeList = attributeList;
    }

    @Override
    public String inspect() {
        return null;
    }

    public String endNotation() {
        return notation.inspect();
    }

    public String text(List<Character> ls, RuntimeContextBuilder.DALRuntimeContext context) {
        TextAttributeListNode.TextAttribute attribute1 = attributeList.getAttribute(context);

        String indent = String.join("", nCopies(notation.getIndent(), " "));
        String text = ls.stream().map(Object::toString).collect(Collectors.joining())
                .substring(notation.getIndent()).replace("\n" + indent, "\n");
        if (text.isEmpty())
            return text;
        return String.join(attribute1.newLine(), lines(text.substring(0, text.length() - 1)));
    }
}
