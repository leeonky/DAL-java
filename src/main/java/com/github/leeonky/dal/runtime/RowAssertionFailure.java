package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.TableNode;
import com.github.leeonky.dal.ast.TransposedTableNode;
import com.github.leeonky.interpreter.InterpreterException;

import static com.github.leeonky.interpreter.FunctionUtil.notAllowParallelReduce;
import static com.github.leeonky.interpreter.InterpreterException.Position.Type.CHAR;

public class RowAssertionFailure extends java.lang.RuntimeException {
    protected final int row;
    protected final DalException dalException;

    public RowAssertionFailure(int row, DalException dalException) {
        this.row = row;
        this.dalException = dalException;
    }

    public DalException linePositionException(TableNode tableNode) {
        dalException.setType(InterpreterException.Position.Type.LINE);
        return dalException;
    }

    public DalException columnPositionException(TransposedTableNode transposedTableNode) {
        dalException.clearPosition();
        return transposedTableNode.transpose().getDataRowByDataIndex(row).getCells().stream()
                .reduce(dalException, this::markPosition, notAllowParallelReduce());
    }

    protected DalException markPosition(DalException e, DALNode cell) {
        return e.multiPosition(cell.getPositionBegin(), CHAR);
    }
}