package com.github.leeonky.dal.ast.node.text;

import com.github.leeonky.dal.ast.node.ConstNode;
import com.github.leeonky.dal.ast.node.DALNode;

import java.util.List;
import java.util.stream.Collectors;

import static java.util.Collections.nCopies;

public class NotationAttribute extends DALNode {
    @Deprecated
    public final String s;

    @Deprecated
    public final TextNotation textNotation;

    public NotationAttribute(DALNode textNotation, DALNode attribute) {
        this.textNotation = (TextNotation) textNotation;
        s = ((ConstNode) attribute).getValue().toString();
        verifyAttribute();
    }

    @Override
    public String inspect() {
        return null;
    }

    public int indent() {
        return textNotation.indent();
    }

    public String endNotation() {
        return String.join("", nCopies((int) s.chars().filter(c -> c == ('`')).count() + 3, "`"));
    }

    public void verifyAttribute() {
        int i = s.indexOf('`');
        String attribute;
        if (i == -1)
            attribute = s;
        else
            attribute = s.substring(i);

        if (attribute.equals("not-exist")) {
            throw textNotation.getSourceCode().syntaxError("Invalid text block attribute `not-exist`, all supported attributes are:\n" +
                                                           "  LF: use \\n as new line", -s.length() - 1);
        }
    }

    public String text(List<Character> ls) {
        String indent = String.join("", nCopies(indent(), " "));
        String text = ls.stream().map(Object::toString).collect(Collectors.joining())
                .substring(indent()).replace("\n" + indent, "\n");
        return text.isEmpty() ? text : text.substring(0, text.length() - 1);
    }
}
