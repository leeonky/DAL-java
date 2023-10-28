Feature: access list element by [n]

  Scenario: all supported build in collection
    Given the following java class:
    """
    public class Bean {
      public String[] array = new String[] {"hello", "world"};
      public List<String> list = Arrays.asList("hello", "world");
      public Collection<String> set = new LinkedHashSet<>(Arrays.asList("hello", "world"));
      public java.util.stream.Stream<String> stream() {
        return Arrays.asList("hello", "world").stream();
      }
      public Iterable<Integer> iterable() {
        return new Iterable<Integer>() {
          int index = 0;
          public Iterator<Integer> iterator() {
            return new Iterator<Integer>() {
                @Override
                public boolean hasNext() {
                  return true;
                }

                @Override
                public Integer next() {
                  return (index++)*2;
                }
            };
          }
        };
      };
    }
    """
    Then the following verification for the instance of java class "Bean" should pass:
    """
    : {
      <<array, set, list, stream>>= [hello world]
      <<array, set, list, stream>>[0]= hello
      <<array, set, list, stream>>[1]= world
      iterable= [0 2 4 6 ...]
      iterable[0]= 0
      iterable[1]= 2
    }
    """

  Scenario Outline: access input list
    Given the following json:
    """
      [1, 2, 3]
    """
    When evaluate by:
    """
      <code>
    """
    Then the result should:
    """
    : <value>
    """
    And the inspect should:
    """
    <inspect>
    """
    Examples:
      | code    | value | inspect |
      | [0]     | 1     | [0]     |
      | [ 1 ]   | 2     | [1]     |
      | [\n2\n] | 3     | [2]     |

  Scenario Outline: access from the end of list
    Given the following json:
    """
      [1, 2, 3]
    """
    When evaluate by:
    """
      <code>
    """
    Then the result should:
    """
    : <value>
    """
    And the inspect should:
    """
    <inspect>
    """
    Examples:
      | code     | value | inspect |
      | [-1]     | 3     | [-1]    |
      | [ -2 ]   | 2     | [-2]    |
      | [\n-3\n] | 1     | [-3]    |

  Scenario: raise error when index out of bound
    Given the following json:
    """
      [1, 2]
    """
    When evaluate by:
    """
      [3]
    """
    Then failed with the message:
    """
    Index out of bounds (3), first index is: 0
    """
    And got the following notation:
    """
      [3]
      ^
    """
    When evaluate by:
    """
      [-3]
    """
    Then failed with the message:
    """
    Index out of bounds (-3), first index is: 0
    """
    And got the following notation:
    """
      [-3]
      ^
    """

  Scenario: raise error when list is null
    When evaluate by:
    """
    null['any']
    """
    Then failed with the message:
    """
    Instance is null
    """
    And got the following notation:
    """
    null['any']
        ^
    """
