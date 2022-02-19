package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.TableNodeBk;

import java.util.List;

public class ElementAssertionFailure extends java.lang.RuntimeException {
    private final int row;
    private final DalException dalException;
    private final List<DALNode> expressions;

    public ElementAssertionFailure(List<DALNode> expressions, int row, DalException dalException) {
        this.row = row;
        this.dalException = dalException;
        this.expressions = expressions;
    }

    public DalException linePositionException() {
        return dalException.multiPosition(expressions.get(row).getOperandPosition(), DalException.Position.Type.LINE);
    }

    public DalException columnPositionException(TableNodeBk tableNode) {
        tableNode.getRows().get(row).getCells().forEach(cell ->
                dalException.multiPosition(cell.getPositionBegin(), DalException.Position.Type.CHAR));
        return dalException;
    }
}
