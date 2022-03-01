package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.InputNode;
import com.github.leeonky.dal.ast.SortSequenceNode;
import com.github.leeonky.dal.compiler.DALProcedure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.interpreter.SyntaxException;

import java.util.Comparator;
import java.util.List;
import java.util.Optional;

import static com.github.leeonky.dal.ast.table.HeaderNode.bySequence;
import static java.util.stream.Collectors.toList;

public class TableHead extends DALNode {
    private final List<HeaderNode> headers;

    public TableHead(List<DALNode> headers) {
        this.headers = headers.stream().map(HeaderNode.class::cast).collect(toList());
    }

    @Override
    public String inspect() {
        return TableNode.printLine(headers);
    }

    public Comparator<Object> collectComparator(RuntimeContextBuilder.DALRuntimeContext context) {
        return headers.stream().sorted(bySequence()).map(headerNode -> headerNode.getListComparator(context))
                .reduce(Comparator::thenComparing).orElse(SortSequenceNode.NOP_COMPARATOR);
    }

    public int size() {
        return headers.size();
    }

    public HeaderNode getHeader(DALProcedure procedure) {
        if (procedure.getIndex() >= headers.size())
            return new HeaderNode(SortSequenceNode.NO_SEQUENCE, InputNode.INSTANCE, Optional.empty());
        return headers.get(procedure.getIndex());
    }

    public void checkSize(RowNode rowNode) {
        if (!rowNode.specialRow() && rowNode.getCells().size() != headers.size())
            throw new SyntaxException("Different cell size",
                    rowNode.getCells().get(rowNode.getCells().size() - 1).getOperandPosition());
    }
}