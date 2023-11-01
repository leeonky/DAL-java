package com.github.leeonky.dal.ast.node.table;

import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.ast.node.ListEllipsisNode;
import com.github.leeonky.dal.ast.node.TableNode;
import com.github.leeonky.dal.ast.node.WildcardNode;
import com.github.leeonky.interpreter.Clause;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static com.github.leeonky.dal.ast.node.InputNode.INPUT_NODE;
import static java.util.Collections.singletonList;

public class TransposedRow extends DALNode {
    private final ColumnHeader columnHeader;
    private final List<Clause<DALNode>> cellClauses;

    public TransposedRow(DALNode header, List<Clause<DALNode>> clauses) {
        cellClauses = clauses;
        columnHeader = (ColumnHeader) header;
        setPositionBegin(header.getPositionBegin());
    }

    @Override
    public String inspect() {
        return TableNode.printLine(new ArrayList<DALNode>() {{
            add(columnHeader);
            addAll(cellClauses.stream().map(clause -> clause.expression(INPUT_NODE)).collect(Collectors.toList())
            );
        }});
    }

    public List<Row> transpose(TransposedRowHeaderRow transposedRowHeaderRow) {
        return new ArrayList<Row>() {{
            for (int i = 0; i < cellClauses.size(); i++)
                add(new Row(transposedRowHeaderRow.getRowHeader(i), singletonList(cellClauses.get(i)),
                        new ColumnHeaderRow(singletonList(columnHeader))));
        }};
    }

    public ColumnHeader getHeader() {
        return columnHeader;
    }

    public int cellCount() {
        return cellClauses.size();
    }

    public void replaceEmptyCell(TransposedRow firstRow) {
        for (int i = 0; i < firstRow.cellClauses.size(); i++) {
            DALNode row = firstRow.cellClauses.get(i).expression(INPUT_NODE);
            if (row instanceof WildcardNode || row instanceof ListEllipsisNode) {
                cellClauses.set(i, node -> new EmptyCell());
            }
        }
    }
}
