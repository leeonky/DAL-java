package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.TableNodeBk;
import com.github.leeonky.dal.ast.table.TableNode;
import com.github.leeonky.dal.ast.table.TransposedTableNode;

import java.util.List;

import static com.github.leeonky.interpreter.InterpreterException.Position.Type.CHAR;

public class ElementAssertionFailure extends java.lang.RuntimeException {
    private final int row;
    private final DalException dalException;
    @Deprecated
    private final List<DALNode> expressions;

    public ElementAssertionFailure(List<DALNode> expressions, int row, DalException dalException) {
        this.row = row;
        this.dalException = dalException;
        this.expressions = expressions;
    }

    public DalException linePositionException() {
        return dalException.multiPosition(expressions.get(row).getOperandPosition(), DalException.Position.Type.LINE);
    }

    public DalException linePositionException(TableNode tableNode) {
        return dalException.multiPosition(tableNode.getRow(row).getPositionBegin(), DalException.Position.Type.LINE);
    }

    @Deprecated
    public DalException columnPositionException(TableNodeBk tableNode) {
        tableNode.getRows().get(row).getCells().forEach(cell ->
                dalException.multiPosition(cell.getPositionBegin(), CHAR));
        return dalException;
    }

    public DalException columnPositionException(TransposedTableNode transposedTableNode) {
        transposedTableNode.transposeRows().get(row).getCells().forEach(cell ->
                dalException.multiPosition(cell.getPositionBegin(), CHAR));
        return dalException;
    }

}
