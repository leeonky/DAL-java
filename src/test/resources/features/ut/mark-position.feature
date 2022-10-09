Feature: mark-position

  Scenario: no mark
    Given the string content:
    """
    hello
    """
    Then got marked string content:
    """
    hello
    """

  Scenario: mark first char position on single line
    Given the string content:
    """
    hello
    """
    When mark an char position on 0
    Then got marked string content:
    """
    hello
    ^
    """

  Scenario: mark last char position on single line
    Given the string content:
    """
    hello
    """
    When mark an char position on 4
    Then got marked string content:
    """
    hello
        ^
    """

  Scenario: mark multi char positions on single line
    Given the string content:
    """
    hello
    """
    When mark an char position on 0
    When mark an char position on 4
    Then got marked string content:
    """
    hello
    ^   ^
    """

  Scenario: mark first char position on first line of multi lines
    Given the string content:
    """
    hello
    world
    """
    When mark an char position on 0
    Then got marked string content:
    """
    hello
    ^
    world
    """

  Scenario: mark last char position on first line of multi lines
    Given the string content:
    """
    hello
    world
    """
    When mark an char position on 4
    Then got marked string content:
    """
    hello
        ^
    world
    """

  Scenario: mark first char position on second line of multi lines
    Given the string content:
    """
    hello
    world
    """
    When mark an char position on 6
    Then got marked string content:
    """
    hello
    world
    ^
    """

  Scenario: mark last char position on second line of multi lines
    Given the string content:
    """
    hello
    world
    """
    When mark an char position on 10
    Then got marked string content:
    """
    hello
    world
        ^
    """

  Scenario: mark char position on second line of multi lines
    Given the string content:
    """
    hello
    world
    """
    When mark an char position on 6
    When mark an char position on 10
    Then got marked string content:
    """
    hello
    world
    ^   ^
    """

  Scenario: mark char position on 3 lines
    Given the string content:
    """
    hello
    world
    !
    """
    When mark an char position on 6
    Then got marked string content:
    """
    hello
    world
    ^
    !
    """

  Scenario: mark char position on multi lines
    Given the string content:
    """
    hello
    world
    !
    """
    When mark an char position on 0
    When mark an char position on 4
    When mark an char position on 6
    When mark an char position on 10
    Then got marked string content:
    """
    hello
    ^   ^
    world
    ^   ^
    !
    """
