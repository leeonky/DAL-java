package com.github.leeonky.dal.ast.opt;

import com.github.leeonky.dal.CompilingContext;
import com.github.leeonky.dal.ast.Node;

public interface Operator {
    Object calculate(CompilingContext context, Node node1, Node node2);

    boolean isMatch(String content);

    int length();
}
