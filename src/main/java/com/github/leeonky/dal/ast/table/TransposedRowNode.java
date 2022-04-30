package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.ListEllipsisNode;
import com.github.leeonky.dal.ast.TableNode;
import com.github.leeonky.dal.ast.WildcardNode;

import java.util.ArrayList;
import java.util.List;

import static java.util.Collections.singletonList;

public class TransposedRowNode extends DALNode {
    private final HeaderNode headerNode;
    private final List<DALNode> cells;

    public TransposedRowNode(DALNode header, List<DALNode> cells) {
        headerNode = (HeaderNode) header;
        this.cells = cells;
        setPositionBegin(header.getPositionBegin());
    }

    @Override
    public String inspect() {
        return TableNode.printLine(new ArrayList<DALNode>() {{
            add(headerNode);
            addAll(cells);
        }});
    }

    public List<RowNode> transpose(TransposedTableHead transposedTableHead) {
        return new ArrayList<RowNode>() {{
            for (int i = 0; i < cells.size(); i++)
                add(new RowNode(transposedTableHead.getPrefix(i), singletonList(cells.get(i))));
        }};
    }

    public HeaderNode getHeader() {
        return headerNode;
    }

    public int cellCount() {
        return cells.size();
    }

    public void replaceEmptyCell(TransposedRowNode transposedRowNode) {
        for (int i = 0; i < transposedRowNode.cells.size(); i++) {
            DALNode row = transposedRowNode.cells.get(i);
            if (row instanceof WildcardNode || row instanceof ListEllipsisNode)
                cells.set(i, new EmptyCellNode());
        }
    }
}
