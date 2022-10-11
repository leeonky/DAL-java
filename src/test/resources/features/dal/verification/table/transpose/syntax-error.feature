Feature: syntax error

  Scenario: different transposed row size
    When evaluate by:
    """
    #
     : >>| name | 'Tom' |
         | age  |
    """
    Then failed with the message:
    """
    Different cell size
    """
    And got the following notation:
    """
    #
     : >>| name | 'Tom' |
    ^^^^^^^^^^^^^^^^^^^^^^
         | age  |
    """

  Scenario: empty table is not allowed
    When evaluate by:
    """
    : >>
    """
    Then failed with the message:
    """
    Expecting a table
    """
    And got the following notation:
    """
    : >>
        ^
    """
    When evaluate by:
    """
    : | >> |
    """
    Then failed with the message:
    """
    Expecting a table
    """
    And got the following notation:
    """
    : | >> |
            ^
    """
    When evaluate by:
    """
    : | >> | 0 |
    """
    Then failed with the message:
    """
    Expecting a table
    """
    And got the following notation:
    """
    : | >> | 0 |
                ^
    """

  Scenario: different row size and prefix size
    When evaluate by:
    """
    #
     : | >>   |
       | name | 'Tom' |
    """
    Then failed with the message:
    """
    Different cell size
    """
    And got the following notation:
    """
    #
     : | >>   |
    ^^^^^^^^^^^^
       | name | 'Tom' |
    """
    When evaluate by:
    """
    #
     : | >>   | 0     |
       | name |
    """
    Then failed with the message:
    """
    Different cell size
    """
    And got the following notation:
    """
    #
     : | >>   | 0     |
    ^^^^^^^^^^^^^^^^^^^^
       | name |
    """

  Scenario: syntax error missing |
    When evaluate by:
    """
    = >>| name
    """
    Then failed with the message:
    """
    Should end with `|`
    """
    And got the following notation:
    """
    = >>| name
              ^
    """

  Scenario: should raise error when invalid table
    When evaluate by:
    """
    = >>| name | ... | 'Tom' | ... | 'Lily' |
        | age  |     | 10    |     | 20     |
    """
    Then failed with the message:
    """
    Invalid ellipsis
    """
    And got the following notation:
    """
    = >>| name | ... | 'Tom' | ... | 'Lily' |
                               ^
        | age  |     | 10    |     | 20     |
    """
    When evaluate by:
    """
    = >>| name | 'Lily' | ... | 'Tom' |
        | age  | 20     |     | 10    |
    """
    Then failed with the message:
    """
    Invalid ellipsis
    """
    And got the following notation:
    """
    = >>| name | 'Lily' | ... | 'Tom' |
                          ^
        | age  | 20     |     | 10    |
    """
    When evaluate by:
    """
    = | >>   | 0     | =      |
      | name | 'Tom' | 'John' |
    """
    Then failed with the message:
    """
    Row index should be consistent
    """
    And got the following notation:
    """
    = | >>   | 0     | =      |
      | name | 'Tom' | 'John' |
               ^       ^
    """
