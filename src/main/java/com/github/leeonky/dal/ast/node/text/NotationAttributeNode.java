package com.github.leeonky.dal.ast.node.text;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.List;

import static com.github.leeonky.dal.util.TextUtil.join;
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

    public Object text(List<Character> content, RuntimeContextBuilder.DALRuntimeContext context) {
        return attributeList.getAttribute(context).format(join(content).substring(notation.getIndent())
                .replace("\n" + String.join("", nCopies(notation.getIndent(), " ")), "\n"));
    }
}
