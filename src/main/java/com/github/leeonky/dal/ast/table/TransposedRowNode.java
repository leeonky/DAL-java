package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALNode;

import java.util.ArrayList;
import java.util.List;

import static java.util.Collections.singletonList;
import static java.util.Optional.empty;
import static java.util.stream.Collectors.toList;

public class TransposedRowNode extends DALNode {
    private final HeaderNode headerNode;
    private final List<DALNode> cells;

    public TransposedRowNode(DALNode header, List<DALNode> cells) {
        headerNode = (HeaderNode) header;
        this.cells = cells;
    }

    @Override
    public String inspect() {
        return TableNode.printLine(new ArrayList<DALNode>() {{
            add(headerNode);
            addAll(cells);
        }});
    }

    //    TODO refactor
    public List<RowNode> transpose(PrefixHeadNode prefixHeadNode) {
        if (prefixHeadNode == null)
            return cells.stream().map(cell -> new RowNode(new RowPrefixNode(empty(), empty(), empty()),
                    singletonList(cell))).collect(toList());

        return new ArrayList<RowNode>() {{
            for (int i = 0; i < cells.size(); i++)
                add(new RowNode(prefixHeadNode.getPrefix(i), singletonList(cells.get(i))));
        }};
    }
}
