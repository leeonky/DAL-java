package com.github.leeonky.dal.ast.node.text;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.dal.runtime.TextBlockAttribute;

import java.util.List;

import static com.github.leeonky.dal.util.TextUtil.join;
import static com.github.leeonky.dal.util.TextUtil.lines;
import static java.util.Collections.nCopies;

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

    public String text(List<Character> content, RuntimeContextBuilder.DALRuntimeContext context) {
        TextBlockAttribute attribute = attributeList.getAttribute(context);
        String indent = String.join("", nCopies(notation.getIndent(), " "));
        String text = join(content).substring(notation.getIndent()).replace("\n" + indent, "\n")
                .replace(attribute.tail() + "\n", "\n");
        return text.isEmpty() ? text : String.join(attribute.newLine(), lines(text.substring(0, text.length() - 1)));
    }
}
