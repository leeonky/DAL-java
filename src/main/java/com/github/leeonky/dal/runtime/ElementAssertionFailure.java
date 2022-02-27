package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.table.TableBody;
import com.github.leeonky.dal.ast.table.TableNode;
import com.github.leeonky.dal.ast.table.TransposedTableNode;

import static com.github.leeonky.interpreter.InterpreterException.Position.Type.CHAR;
import static com.github.leeonky.interpreter.InterpreterException.Position.Type.LINE;

public class ElementAssertionFailure extends java.lang.RuntimeException {
    private final int row;
    private final DalException dalException;

    public ElementAssertionFailure(int row, DalException dalException) {
        this.row = row;
        this.dalException = dalException;
    }

    public DalException linePositionException(TableNode tableNode) {
        return dalException.multiPosition(tableNode.getDataRowByDataIndex(row).getPositionBegin(), LINE);
    }

    public DalException columnPositionException(TransposedTableNode transposedTableNode) {
        new TableBody(transposedTableNode.transposeRows()).getDataRowByDataIndex(row).getCells().forEach(cell ->
                dalException.multiPosition(cell.getPositionBegin(), CHAR));
        return dalException;
    }

}
