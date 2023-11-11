package com.github.leeonky.dal.runtime;

import com.github.leeonky.dal.ast.node.TableNode;
import com.github.leeonky.dal.ast.node.TransposedTableNode;

import static com.github.leeonky.interpreter.InterpreterException.Position.Type.ROW;

public class ElementAssertionFailure extends RowAssertionFailure {

    public ElementAssertionFailure(int indexSkipEllipsis, DalException dalException) {
        super(indexSkipEllipsis, dalException);
    }

    @Override
    public DalException linePositionException(TableNode tableNode) {
        return dalException.multiPosition(tableNode.fetchDataRowSkipEllipsis(indexSkipEllipsis).getPositionBegin(), ROW);
    }

    @Override
    public DalException columnPositionException(TransposedTableNode transposedTableNode) {
        return transposedTableNode.transpose().fetchDataRowSkipEllipsis(indexSkipEllipsis).markPositionOnCells(dalException);
    }
}
