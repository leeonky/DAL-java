package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.TableNode;
import com.github.leeonky.dal.ast.TransposedTableNode;
import com.github.leeonky.interpreter.InterpreterException;

public class RowAssertionFailure extends java.lang.RuntimeException {
    protected final int indexSkipEllipsis;
    protected final DalException dalException;

    public RowAssertionFailure(int indexSkipEllipsis, DalException dalException) {
        this.indexSkipEllipsis = indexSkipEllipsis;
        this.dalException = dalException;
    }

    public DalException linePositionException(TableNode tableNode) {
        dalException.setType(InterpreterException.Position.Type.LINE);
        return dalException;
    }

    public DalException columnPositionException(TransposedTableNode transposedTableNode) {
        dalException.clearPosition();
        return transposedTableNode.transpose().fetchDataRowSkipEllipsis(indexSkipEllipsis).markPositionOnCells(dalException);
    }
}
