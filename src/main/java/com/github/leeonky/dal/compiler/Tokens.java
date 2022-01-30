package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.DALExpression;
import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.interpreter.SourceCode;
import com.github.leeonky.interpreter.Token;
import com.github.leeonky.interpreter.TokenScanner;

import java.util.HashSet;

import static com.github.leeonky.dal.compiler.Constants.*;
import static com.github.leeonky.interpreter.FunctionUtil.not;
import static com.github.leeonky.interpreter.SourceCode.tokenScanner;
import static java.util.Arrays.asList;
import static java.util.Collections.emptySet;

public class Tokens {
    public static final TokenScanner<RuntimeContextBuilder.DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            NUMBER = tokenScanner(DIGITAL::contains, emptySet(), false, (lastChar, nextChar) ->
            ((lastChar != 'e' && lastChar != 'E') || (nextChar != '-' && nextChar != '+')) && DELIMITER.contains(nextChar), Token::isNumber),
            INTEGER = SourceCode.tokenScanner(DIGITAL_OR_MINUS::contains, emptySet(), false, DELIMITER, Token::isNumber),
            IDENTITY_PROPERTY = SourceCode.tokenScanner(not(DELIMITER::contains), ALL_KEY_WORDS, false, DELIMITER_OR_DOT, not(Token::isNumber)),
            DOT_PROPERTY = SourceCode.tokenScanner(DOT::equals, new HashSet<>(asList(ELEMENT_ELLIPSIS, DOT.toString())), true, DELIMITER_OR_DOT);

    public static final TokenScanner.Mandatory SCHEMA = SourceCode.tokenScanner(not(DELIMITER::contains), ALL_KEY_WORDS,
            false, DELIMITER, not(Token::isNumber)).mandatory("expect a schema");

}
