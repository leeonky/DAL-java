package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.token.SourceCode;
import com.github.leeonky.dal.token.Token;

import java.util.ArrayList;
import java.util.List;

class ParseContext {
    //TODO to be private
    final SourceCode sourceCode;
    final Token last;
    final List<Character> content = new ArrayList<>();

    ParseContext(SourceCode sourceCode, Token last) {
        this.sourceCode = sourceCode;
        this.last = last;
    }

    public boolean isLastTokenOperatorMatches() {
        return last != null && last.isOperatorMatches();
    }
}
