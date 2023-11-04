Feature: const remark

  Rule: verification

    Scenario: const remark for const value node
      When evaluate by:
      """
      2 (1+1)
      """
      Then the result should:
      """
      : 2
      """
      And the inspect should:
      """
      2 (1 + 1)
      """

    Scenario: incorrect value in const remark
      When evaluate by:
      """
      2 (1+2)
      """
      Then failed with the message:
      """
      Incorrect const remark, const value was java.lang.Integer
      <2>
      but remark (1 + 2) was java.lang.Integer
      <3>
      """
      And got the following notation:
      """
      2 (1+2)
        ^
      """

    Scenario: const remark for negative const value node
      Then the following verification should pass:
      """
      -2(-1-1)= -2
      """
      Given the following json:
      """
      -2
      """
      Then the following verification should pass:
      """
      = -2 (1-3)
      """

  Rule: in expression

    Scenario: N/A for parentheses
      When evaluate by:
      """
      (1+1)(2)
      """
      Then failed with the message:
      """
      more than one expression
      """
      And got the following notation:
      """
      (1+1)(2)
           ^
      """

    Scenario: N/A after property
      Given the following json:
      """
      [2]
      """
      When evaluate by:
      """
      .0(2)
      """
      Then failed with the message:
      """
      Expect a symbol
      """
      And got the following notation:
      """
      .0(2)
       ^
      """
      When evaluate by:
      """
      [0](2)
      """
      Then failed with the message:
      """
      Not implement operator () of java.lang.Integer
      """
      And got the following notation:
      """
      [0](2)
         ^
      """
      When evaluate by:
      """
      : {
      0(2): 2
      }
      """
      Then failed with the message:
      """
      Not implement operator () of java.lang.Integer
      """
      And got the following notation:
      """
      : {
      0(2): 2
       ^
      }
      """

    Scenario: before property
      Then the following verification should pass:
      """
      2(1+1).toString= '2'
      """

    Scenario: N/A for negative
      When evaluate by:
      """
      !2(-2)
      """
      Then failed with the message:
      """
      Incorrect const remark, const value was java.lang.Integer
      <2>
      but remark (-2) was java.lang.Integer
      <-2>
      """
      And got the following notation:
      """
      !2(-2)
        ^
      """

    Scenario: with arithmetic * / + -
      Then the following verification should pass:
      """
      10 * 2(1+1)= 20,
      10 / 2(1+1)= 5
      """
      Then the following verification should pass:
      """
      2(1+1) * 10= 20,
      10(5+5) / 2= 5
      """
      Then the following verification should pass:
      """
      10 + 2(1+1)= 12,
      10 - 2(1+1)= 8
      """
      Then the following verification should pass:
      """
      2(1+1) + 10= 12,
      10(5+5) - 2= 8
      """

    Scenario: with comparison > >= < <= !=
      Then the following verification should pass:
      """
      10 > 2(1+1)= true,
      10 >= 2(1+1)= true,
      1 < 2(1+1)= true,
      1 <= 2(1+1)= true,
      1 != 2(1+1)= true
      """
      Then the following verification should pass:
      """
      2(1+1) < 10= true,
      2(1+1) <= 10= true,
      2(1+1) > 1= true,
      2(1+1) >= 1= true,
      2(1+1) != 1= true
      """

    Scenario: with logical && || and or
      Then the following verification should pass:
      """
      1 && 2(1+1)= 2,
      false || 2(1+1)= 2,
      1 and 2(1+1)= 2,
      false or 2(1+1)= 2
      """
      Then the following verification should pass:
      """
      2(1+1) && 1= 1,
      2(1+1) || false= 2,
      2(1+1) and 1= 1,
      2(1+1) or false= 2
      """

    Scenario: with verification = :
      Then the following verification should pass:
      """
      2(1+1)= 2,
      2(1+1): 2
      """
      Then the following verification should pass:
      """
      2= 2(1+1),
      2: 2(1+1)
      """
