package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALExpression;
import com.github.leeonky.dal.ast.SortGroupNode;
import com.github.leeonky.dal.ast.SymbolNode;
import com.github.leeonky.dal.ast.opt.Factory;
import com.github.leeonky.dal.compiler.Notations;

import static com.github.leeonky.dal.ast.InputNode.INPUT_NODE;
import static java.util.Collections.emptyList;
import static java.util.Optional.empty;

public class TableDefaultIndexHeadRow extends TableHeadRow {

    public TableDefaultIndexHeadRow() {
        super(emptyList());
    }

    @Override
    public HeaderNode getHeader(int index) {
        return new HeaderNode(SortGroupNode.NO_SEQUENCE, new DALExpression(INPUT_NODE, Factory.executable(Notations.EMPTY),
                new SymbolNode(index, SymbolNode.Type.NUMBER)), empty());
    }

    @Override
    public void checkDataCellSize(TableRowNode rowNode) {
    }

    @Override
    public String inspect() {
        return "^";
    }
}
