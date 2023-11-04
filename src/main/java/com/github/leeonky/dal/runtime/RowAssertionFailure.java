package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.node.TableNode;
import com.github.leeonky.dal.ast.node.TransposedTableNode;
import com.github.leeonky.interpreter.InterpreterException;

public class RowAssertionFailure extends java.lang.RuntimeException {
    protected final int indexSkipEllipsis;
    protected final DALException dalException;

    public RowAssertionFailure(int indexSkipEllipsis, DALException dalException) {
        this.indexSkipEllipsis = indexSkipEllipsis;
        this.dalException = dalException;
    }

    public DALException linePositionException(TableNode tableNode) {
        dalException.setType(InterpreterException.Position.Type.ROW);
        return dalException;
    }

    public DALException columnPositionException(TransposedTableNode transposedTableNode) {
        dalException.clearPosition();
        return transposedTableNode.transpose().fetchDataRowSkipEllipsis(indexSkipEllipsis).markPositionOnCells(dalException);
    }
}
