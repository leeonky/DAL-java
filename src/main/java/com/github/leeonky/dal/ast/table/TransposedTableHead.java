package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.TableNode;
import com.github.leeonky.interpreter.InterpreterException;
import com.github.leeonky.interpreter.SyntaxException;

import java.util.List;
import java.util.stream.Collectors;

import static java.util.Optional.empty;

public class TransposedTableHead extends DALNode {
    private final List<TableRowPrefixNode> prefixes;

    public TransposedTableHead(List<DALNode> prefixes) {
        this.prefixes = prefixes.stream().map(TableRowPrefixNode.class::cast).collect(Collectors.toList());
        if (!prefixes.isEmpty())
            setPositionBegin(prefixes.get(prefixes.size() - 1).getPositionBegin());
    }

    @Override
    public String inspect() {
        return "| >> " + TableNode.printLine(prefixes) + "\n";
    }

    public TableRowPrefixNode getPrefix(int i) {
        if (i >= prefixes.size())
            return new TableRowPrefixNode(empty(), empty(), empty());
        return prefixes.get(i);
    }

    public void checkSize(TransposedRowNode r) {
        if (r.cellCount() != prefixes.size())
            throw new SyntaxException("Different cell size", getPositionBegin(), InterpreterException.Position.Type.LINE);
    }
}
