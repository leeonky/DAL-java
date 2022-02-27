package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALNode;

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
}
