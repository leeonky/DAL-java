package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.interpreter.InterpreterException;
import com.github.leeonky.interpreter.SyntaxException;

import java.util.List;
import java.util.stream.Collectors;

public class TransposedTableHead extends DALNode {
    private final List<RowPrefixNode> prefixes;

    public TransposedTableHead(List<DALNode> prefixes) {
        this.prefixes = prefixes.stream().map(RowPrefixNode.class::cast).collect(Collectors.toList());
        if (!prefixes.isEmpty())
            setPositionBegin(prefixes.get(prefixes.size() - 1).getPositionBegin());
    }

    @Override
    public String inspect() {
        return "| >> " + TableNode.printLine(prefixes) + "\n";
    }

    public RowPrefixNode getPrefix(int i) {
        if (i >= prefixes.size())
            throw new SyntaxException("Different cell size", getPositionBegin(), InterpreterException.Position.Type.LINE);
        return prefixes.get(i);
    }

    public void checkSize(TransposedRowNode r) {
        if (r.cellCount() != prefixes.size())
            throw new SyntaxException("Different cell size", r.getPositionBegin(), InterpreterException.Position.Type.LINE);
    }
}
