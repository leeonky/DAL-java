Feature: list

  Rule: fixed length list

    Scenario: empty list
      Given the following json:
      """
      []
      """
      Then the following verification should pass:
      """
      : []
      """
      And the following verification should pass:
      """
      = []
      """

    Scenario: one element
      Given the following json:
      """
      [1]
      """
      Then the following verification should pass:
      """
      : [1]
      """
      When evaluate by:
      """
      : [1 2]
      """
      Then failed with the message:
      """
      Different list size
      Expected: <2>
      Actual: <1>
      """
      And got the following notation:
      """
      : [1 2]
        ^
      """
      When evaluate by:
      """
      : []
      """
      Then failed with the message:
      """
      Different list size
      Expected: <0>
      Actual: <1>
      """
      And got the following notation:
      """
      : []
        ^
      """
      When evaluate by:
      """
      : [2]
      """
      Then failed with the message:
      """
      Expected to match: java.lang.Integer
      <2>
       ^
      Actual: java.lang.Integer
      <1>
       ^
      """
      And got the following notation:
      """
      : [2]
         ^
      """

    Scenario: default verification operator
      Given the following json:
      """
      [ 1 ]
      """
      When evaluate by:
      """
       = [ /1/ ]
      """
      Then failed with the message:
      """
      Operator = before regex need a string input value
      """
      And got the following notation:
      """
       = [ /1/ ]
       ^
      """

    Scenario: two element
      Given the following json:
      """
      [1, 2]
      """
      Then the following verification should pass:
      """
      : [1 2]
      """
      And the inspect should:
      """
      : [[0]: 1, [1]: 2]
      """
      When evaluate by:
      """
      : [1 3]
      """
      Then failed with the message:
      """
      Expected to match: java.lang.Integer
      <3>
       ^
      Actual: java.lang.Integer
      <2>
       ^
      """
      And got the following notation:
      """
      : [1 3]
           ^
      """

    Scenario: raise error when no closing bracket
      When evaluate by:
      """
      : [1
      """
      Then failed with the message:
      """
      Should end with `]`
      """
      And got the following notation:
      """
      : [1
          ^
      """

    Scenario: support incomplete List
      Given the following json:
      """
      [ 1, 2, 3 ]
      """
      Then the following verification should pass:
      """
      : [1 ...]
      """
      And the inspect should:
      """
      : [[0]: 1, ...]
      """
      And the following verification should pass:
      """
      : [... 3]
      """
      And the inspect should:
      """
      : [..., [-1]: 3]
      """

    Scenario: should raise error when invalid incomplete List
      When evaluate by:
      """
      : [... 1 ... 2]
      """
      Then failed with the message:
      """
      Invalid ellipsis
      """
      And got the following notation:
      """
      : [... 1 ... 2]
               ^
      """
      When evaluate by:
      """
      : [0 ... 1 ...]
      """
      When evaluate by:
      """
      : [0 ... 1]
      """
      Then failed with the message:
      """
      Invalid ellipsis
      """
      And got the following notation:
      """
      : [0 ... 1]
           ^
      """

    Scenario: support different verification operator in element
      Given the following json:
      """
      [ 1 ]
      """
      Then the following verification should pass:
      """
      = [ : /1/ ]
      """
      And the inspect should:
      """
      = [[0]: /1/]
      """

    Scenario Outline: support expression element
      Given the following json:
      """
      [ 3 ]
      """
      Then the following verification should pass:
      """
      : [ <opt>1+2 ]
      """
      And the inspect should:
      """
      : [[0]<inspect> 1 + 2]
      """
      Examples:
        | opt | inspect |
        |     | :       |
        | :   | :       |
        | =   | =       |

    Scenario: optional comma
      Given the following json:
      """
      [1, 2, 3]
      """
      Then the following verification should pass:
      """
      :[1, 2 3,]
      """
      And the inspect should:
      """
      : [[0]: 1, [1]: 2, [2]: 3]
      """

    Scenario: comma as logic and in parentheses
      Given the following json:
      """
      [ true, false ]
      """
      Then the following verification should pass:
      """
      : [(true,true), false]
      """

    Scenario: support schema expression
      Given the following json:
      """
      [ "1", 2 ]
      """
      Then the following verification should pass:
      """
      : [ is String, is Number ]
      """
      And the inspect should:
      """
      : [[0] is String, [1] is Number]
      """
      Given the following json:
      """
      [ "1" ]
      """
      And the following verification should failed:
      """
      : [ is Integer ]
      """
      And got the following notation:
      """
      : [ is Integer ]
             ^
      """
      And the following verification should pass:
      """
      : [ is String: {length: 1} ]
      """
      And the inspect should:
      """
      : [[0] is String: {length: 1}]
      """

    Scenario: change first element index
      Given the following java class:
      """
      public class Table extends java.util.ArrayList<Row> {
        public Table() {
          Row row = new Row();
          row.v1 = 11;
          row.v2 = 12;
          add(row);

          row = new Row();
          row.v1 = 21;
          row.v2 = 22;
          add(row);
        }
      }
      """
      And the following java class:
      """
      public class Row {
        public int v1, v2;
      }
      """
      And set the first element index to 1 of list "Table"
      Then use a instance of java class "Table" to evaluate:
      """
      : {
        [1]: {
          v1: 11
          v2: 12
        }
        [2]: {
          v1: 21
          v2: 22
        }
      }
      """
      And the following verification should pass:
      """
      [-2]: {
        v1: 11
        v2: 12
      }
      """
      And the following verification should pass:
      """
      : | v1 | v2 |
      1 | 11 | 12 |
      2 | 21 | 22 |
      """
      And the following verification should pass:
      """
      :   | >> |  1 |  2 |
          | v1 | 11 | 21 |
          | v2 | 12 | 22 |
      """
      And the following verification should pass:
      """
      : [{
        v1: 11
        v2: 12
      }{
        v1: 21
        v2: 22
      }]
      """
      And the following verification should pass:
      """
      : [{
        v1: 11
        v2: 12
      } ...]
      """
      And the following verification should pass:
      """
      : [... {
        v1: 21
        v2: 22
      }]
      """
      And the following verification should pass:
      """
      : | v1 | v2 |
        | 11 | 12 |
        | 21 | 22 |
      """
      And the following verification should pass:
      """
      : >>| v1 | 11 | 21 |
          | v2 | 12 | 22 |
      """

    Scenario: list "contains"
      Given the following json:
      """
      [1, 2, 3, 4]
      """
      Then the following verification should pass:
      """
      : [... 1 3 ...]
      """
      And the inspect should:
      """
      : [..., : 1, : 3, ...]
      """
      When evaluate by:
      """
      : [... 1 5 ...]
      """
      Then failed with the message:
      """
      No such element
      """
      And got the following notation:
      """
      : [... 1 5 ...]
               ^
      """

    Scenario: list contains object
      Given the following json:
      """
      [{
        "name": "Tom",
        "age": 18
      }, {
        "name": "John",
        "age": 20
      }]
      """
      Then the following verification should pass:
      """
      : [... {
        name: Tom
      } ...]
      """
      And the inspect should:
      """
      : [..., : {name: 'Tom'}, ...]
      """
      When evaluate by:
      """
      : [...
      {
        name: John
      } {
        age: 30
      }
      ...]
      """
      Then failed with the message:
      """
      No such element
      """
      And got the following notation:
      """
      : [...
      {
        name: John
      } {
        ^
        age: 30
      }
      ...]
      """

    Scenario: sort list
      Given the following json:
      """
      [1, 3, 2]
      """
      When evaluate by:
      """
      (+{})
      """
      Then the result should:
      """
      : [1, 2, 3]
      """
      And the inspect should:
      """
      (+{})
      """
      When evaluate by:
      """
      (-{})
      """
      Then the result should:
      """
      : [3, 2, 1]
      """
      And the inspect should:
      """
      (-{})
      """

    Scenario: sort list in object scope verification
      Given the following json:
      """
      {
        "items": [1, 3, 2]
      }
      """
      And the following verification should pass:
      """
      : {
        items: +[1 2 3]
        items: -[3 2 1]
      }
      """
      And the inspect should:
      """
      : {items: +[[0]: 1, [1]: 2, [2]: 3], items: -[[0]: 3, [1]: 2, [2]: 1]}
      """

    Scenario: sort list in nested list
      Given the following json:
      """
      [
        [1, 3, 2],
        [1, 3, 2]
      ]
      """
      And the following verification should pass:
      """
      : [
        +[1 2 3]
        -[3 2 1]
      ]
      """
      And the inspect should:
      """
      : [[0]: +[[0]: 1, [1]: 2, [2]: 3], [1]: -[[0]: 3, [1]: 2, [2]: 1]]
      """

    Scenario: support access infinite collection by index
      Given the following java class:
      """
      public class InfiniteCollection implements Iterable<Integer> {
        private int index=0;

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
      }
      """
      Then the following verification for the instance of java class "InfiniteCollection" should pass:
      """
      : {
        [0]= 0,
        [1]= 2,
        [2]= 4
      }
      """
      And the following verification for the instance of java class "InfiniteCollection" should pass:
      """
      : | toString |
        | '0'      |
        | '2'      |
        | '4'      |
        | ...      |
      """

    Scenario: sort list and verification should inherit origin list first index and schema
      Given the following java class:
      """
      public class Bean implements Comparable<Bean>{
        public final String id;
        public Bean(String v) {
          this.id=v;
        }

        public int compareTo(Bean o) {
          return id.compareTo(o.id);
        }
      }
      """
      Given the following java class:
      """
      public class BeanList extends ArrayList<Bean> {
        public BeanList() {
          add(new Bean("a"));
          add(new Bean("c"));
          add(new Bean("b"));
        }
      }
      """
      And the following schema class:
      """
      @Partial
      @FieldAliases({
              @FieldAlias(alias = "aliasOfId", field = "id")
      })
      public class BeanSchema implements Schema{
      }
      """
      And register the following BeanDALCollectionFactory for java class "BeanList":
      """
      public class BeanDALCollectionFactory implements DALCollectionFactory<BeanList, Bean> {
        public DALCollection<Bean> create(BeanList list) {
          return new CollectionDALCollection<Bean>(list) {
            public int firstIndex() {
              return 1;
            }
          };
        }
      }
      """
      Then the following verification for the instance of java class "BeanList" should pass:
      """
      (+({} is [BeanSchema]))[2].aliasOfId= b
      """
      Then the following verification for the instance of java class "BeanList" should pass:
      """
      (+({} is [BeanSchema])): | aliasOfId |
                             2 | b         |
                            -1 | c         |
      """
      Then the following verification for the instance of java class "BeanList" should pass:
      """
      is [BeanSchema]: +[{aliasOfId= a}{aliasOfId= b}{aliasOfId= c}]
      """
      Then the following verification for the instance of java class "BeanList" should pass:
      """
      is [BeanSchema]:  | +aliasOfId |
                      2 | b          |
                     -1 | c          |
      """

  Rule: infinite collection

    Background:
      Given the following java class:
      """
      public class InfiniteCollection implements Iterable<Integer> {
        private int index=0;
        public Iterator<Integer> iterator() {
          return new Iterator<Integer>() {
            public boolean hasNext() {
              return true;
            }
            public Integer next() {
              return (index++)*2;
            }
          };
        }
      }
      """
      Given register the following BeanDALCollectionFactory for java class "InfiniteCollection":
      """
      public class BeanDALCollectionFactory implements DALCollectionFactory<InfiniteCollection, Integer> {
        public DALCollection<Integer> create(InfiniteCollection list) {
          return new IterableDALCollection<Integer>(list) {
            public boolean infinite() {
              return true;
            }
          };
        }
      }
      """

    Scenario: set infinite flag and should raise error when try to get size
      When use a instance of java class "InfiniteCollection" to evaluate:
      """
      ::size= 0
      """
      Then failed with the message:
      """
      Not supported for infinite collection
      """
      And got the following notation:
      """
      ::size= 0
        ^
      """

    Scenario: set infinite flag and should raise error when try to sort list
      When use a instance of java class "InfiniteCollection" to evaluate:
      """
      (+{})
      """
      Then failed with the message:
      """
      Can not sort infinite collection
      """
      And got the following notation:
      """
      (+{})
       ^
      """
      When use a instance of java class "InfiniteCollection" to evaluate:
      """
      (-{})
      """
      Then failed with the message:
      """
      Can not sort infinite collection
      """
      And got the following notation:
      """
      (-{})
       ^
      """

    Scenario: set infinite flag and should raise error when try to verify list schema
      When use a instance of java class "InfiniteCollection" to evaluate:
      """
      is [String]
      """
      Then failed with the message:
      """
      Not supported for infinite collection
      """
      And got the following notation:
      """
      is [String]
         ^
      """

    Scenario: set infinite flag and should raise error when use negative index
      When use a instance of java class "InfiniteCollection" to evaluate:
      """
      [-1]
      """
      Then failed with the message:
      """
      Get property `-1` failed, property can be:
        1. public field
        2. public getter
        3. public no args method
        4. Map key value
        5. customized type getter
        6. static method extension
      Not support negative index in infinite collection
      Implicit list mapping is not allowed in current version of DAL, use `-1[]` instead
      """
      And got the following notation:
      """
      [-1]
      ^
      """

    Scenario: set infinite flag and should ignore list size in verification (multi column make sure merged NOP_COMPARATOR is also NOP_COMPARATOR)
      Then the following verification for the instance of java class "InfiniteCollection" should pass:
      """
      {}= [0 2 4], {}= [0 2 4 6],

      {}: | intValue | longValue |
          | 0        | 0         |
          | 2        | 2         |
          | 4        | 4         |
      ,

      {}= | intValue |
          | 0        |
          | 2        |
          | ...      |
      ,

      {}= | intValue |
          | 0        |
          | 2        |
          | 4        |
          | 6        |
      """

#  TODO contains not allow set index or key in table
#  TODO try to support [1 ... 2] ?
