package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.*;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Clause;
import com.github.leeonky.interpreter.ClauseParser;
import com.github.leeonky.interpreter.NodeParser;

import java.util.List;
import java.util.stream.Collectors;

public class SchemaClauseMandatory implements ClauseParser.Mandatory<DALRuntimeContext, DALNode, DALExpression,
        DALOperator, DALProcedure> {
    public static final NodeParser.Mandatory<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> SCHEMA;

    static {
        SCHEMA = Tokens.SCHEMA.map(token -> new SchemaNode(token.getContent()));
    }

    @Override
    public Clause<DALRuntimeContext, DALNode> parse(DALProcedure procedure) {
        return fetchSchemaClause(procedure, 0);
    }

    private Clause<DALRuntimeContext, DALNode> fetchSchemaClause(DALProcedure dalProcedure, int dimension) {
        if (dimension > 1)
            throw dalProcedure.getSourceCode().syntaxError("Not support multidimensional schema", 0);
        return dalProcedure.fetchBetween("[", "]", () -> fetchSchemaClause(dalProcedure, dimension + 1)).orElseGet(() -> {
            List<SchemaNode> schemaNodes = dalProcedure.fetchNodesSplitBy("/", SCHEMA).stream()
                    .map(SchemaNode.class::cast).collect(Collectors.toList());
            return input -> new SchemaExpression(input, schemaNodes, dimension);
        });
    }
}
