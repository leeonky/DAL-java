package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Clause;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static java.util.Collections.singletonList;

public class TransposedRowNode extends DALNode {
    private final HeaderNode headerNode;
    private final List<Clause<DALRuntimeContext, DALNode>> cellClauses;

    public TransposedRowNode(DALNode header, List<Clause<DALRuntimeContext, DALNode>> clauses) {
        cellClauses = clauses;
        headerNode = (HeaderNode) header;
        setPositionBegin(header.getPositionBegin());
    }

    @Override
    public String inspect() {
        return TableNode.printLine(new ArrayList<DALNode>() {{
            add(headerNode);
//            TODO use clause
            addAll(cellClauses.stream().map(clause -> clause.expression(InputNode.INSTANCE)).collect(Collectors.toList())
            );
        }});
    }

    public List<TableRowNode> transpose(TransposedTableHead transposedTableHead) {
        return new ArrayList<TableRowNode>() {{
            for (int i = 0; i < cellClauses.size(); i++)
                add(new TableRowNode(transposedTableHead.getPrefix(i), singletonList(cellClauses.get(i)),
                        new TableHeadRow(singletonList(headerNode))));
        }};
    }

    public HeaderNode getHeader() {
        return headerNode;
    }

    public int cellCount() {
        return cellClauses.size();
    }

    public void replaceEmptyCell(TransposedRowNode firstRow) {
        for (int i = 0; i < firstRow.cellClauses.size(); i++) {
//            TODO refactor
            DALNode row = firstRow.cellClauses.get(i).expression(null);
            if (row instanceof WildcardNode || row instanceof ListEllipsisNode) {
                cellClauses.set(i, node -> new EmptyCellNode());
            }
        }
    }
}
