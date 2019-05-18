package com.github.leeonky.dal.ast.opt;

import com.github.leeonky.dal.CompilingContext;
import com.github.leeonky.dal.DALCompiler;
import com.github.leeonky.dal.ast.Node;

import java.util.Objects;

public class Is implements Operator {

    @Override
    public Object calculate(CompilingContext context, Node node1, Node node2) {
        return Objects.equals(node1.evaluate(context), node2.evaluate(context));
    }

    @Override
    public boolean isMatch(String content) {
        return content.startsWith("is") && DALCompiler.isSpliter(content, length());
    }

    @Override
    public int length() {
        return "is".length();
    }

}
