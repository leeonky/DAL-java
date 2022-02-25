package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALNode;

import java.util.List;
import java.util.stream.Collectors;

public class PrefixHeadNode extends DALNode {
    private final List<RowPrefixNode> prefixes;

    public PrefixHeadNode(List<DALNode> prefixes) {
        this.prefixes = prefixes.stream().map(RowPrefixNode.class::cast).collect(Collectors.toList());
    }

    @Override
    public String inspect() {
        return "| >> " + TableNode.printLine(prefixes);
    }

    public RowPrefixNode getPrefix(int i) {
        return prefixes.get(i);
    }
}
