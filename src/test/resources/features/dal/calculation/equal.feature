Feature: equal

  Rule: null

    Scenario: compare primitive null
      Then the following verification should pass:
      """
      null= null
      """
      When evaluate by:
      """
      1= null
      """
      Then failed with the message:
      """
      Expected to be equal to: null
                               ^
      Actual: java.lang.Integer
              ^
      <1>
      """
      And got the following notation:
      """
      1= null
         ^
      """
      When evaluate by:
      """
      null= 1
      """
      Then failed with the message:
      """
      Expected to be equal to: java.lang.Integer
                               ^
      <1>
      Actual: null
              ^
      """
      And got the following notation:
      """
      null= 1
            ^
      """

    Scenario: compare customized null
      Given the following java class:
      """
      public class Bean {
      }
      """
      And the following java class:
      """
      public class BeanContainer {
        public Bean bean1 = new Bean();
        public Bean bean2 = new Bean();
      }
      """
      And register DAL:
      """
      dal.getRuntimeContextBuilder()
        .registerPropertyAccessor(Bean.class, new JavaClassPropertyAccessor<Bean>(BeanClass.create(Bean.class)) {
          public boolean isNull(Bean instance) {
              return true;
          }
        });
      """
      Then the following verification for the instance of java class "BeanContainer" should pass:
      """
      bean1= .bean2
      """
      And the following verification for the instance of java class "BeanContainer" should pass:
      """
      null= .bean2
      """
      And the following verification for the instance of java class "BeanContainer" should pass:
      """
      bean1= null
      """

  Rule: list

    Background:
      Given the following java class:
      """
      public class ListObject {
        int index = 0;
        public int get() {
          return (index++) * 2;
        }
      }
      """
      And register DAL:
      """
      dal.getRuntimeContextBuilder().registerDALCollectionFactory(ListObject.class, list->
          new IterableDALCollection<>(java.util.stream.Stream.generate(list::get).limit(5)::iterator));
      """

    Scenario: compare primitive list
      Given the following json:
      """
      {
        "list": [],
        "empty": [],
        "list1": [1]
      }
      """
      Then the following verification should pass:
      """
      list= .empty
      """
      When evaluate by:
      """
      list= .list1
      """
      Then failed with the message:
      """
      Expected to be equal to: [
                                ^
          java.lang.Integer <1>
      ]
      Actual: []
               ^
      """
      And got the following notation:
      """
      list= .list1
            ^
      """

    Scenario: compare customized list
      Given the following java class:
      """
      public class Container {
        public ListObject list1 = new ListObject();
        public ListObject list2 = new ListObject();

        public ListObject list3 = new ListObject();
        public ListObject list4 = new ListObject();
        public int[] list5 = new int[] {0, 2, 4, 6, 8};
      }
      """
      When the following verification for the instance of java class "Container" should pass:
      """
      : {
        list1= .list2
        list3= .list5
        list5= .list4
      }
      """

    Scenario: use object compare when another object is not list
      Given the following java class:
      """
      public class Container {
        public ListObject list1 = new ListObject();
        public ListObject list2 = new ListObject();
      }
      """
      When use a instance of java class "Container" to evaluate:
      """
      list1= not-list
      """
      Then failed with the message:
      """
      Expected to be equal to: java.lang.String
                               ^
      <not-list>
      Actual: #package#ListObject [
              ^
          java.lang.Integer <0>,
          java.lang.Integer <2>,
          java.lang.Integer <4>,
          java.lang.Integer <6>,
          java.lang.Integer <8>
      ]
      """
      And got the following notation:
      """
      list1= not-list
             ^
      """
      When use a instance of java class "Container" to evaluate:
      """
      'not-list'= .list2
      """
      Then failed with the message:
      """
      Expected to be equal to: #package#ListObject [
                               ^
          java.lang.Integer <0>,
          java.lang.Integer <2>,
          java.lang.Integer <4>,
          java.lang.Integer <6>,
          java.lang.Integer <8>
      ]
      Actual: java.lang.String
              ^
      <not-list>
      """
      And got the following notation:
      """
      'not-list'= .list2
                  ^
      """

    Scenario: equals when same infinite list instance
      Given the following java class:
      """
      public class Container {
        public ListObject list1 = new ListObject();
      }
      """
      And register DAL:
      """
      dal.getRuntimeContextBuilder().registerDALCollectionFactory(ListObject.class, list->
          new IterableDALCollection<Integer>(java.util.stream.Stream.generate(list::get).limit(5)::iterator){
            public boolean infinite() {
                return true;
            }
          });
      """
      Then the following verification for the instance of java class "Container" should pass:
      """
      list1= .list1
      """

    Scenario: not support infinite list instance
      Given the following java class:
      """
      public class Container {
        public ListObject list = new ListObject();
        public int[] list2 = new int[] {0, 2, 4, 6, 8};
      }
      """
      And register DAL:
      """
      dal.getRuntimeContextBuilder().registerDALCollectionFactory(ListObject.class, list->
          new IterableDALCollection<Integer>(java.util.stream.Stream.generate(list::get).limit(5)::iterator){
            public boolean infinite() {
                return true;
            }
          });
      """
      When use a instance of java class "Container" to evaluate:
      """
      list= .list2
      """
      Then failed with the message:
      """
      Invalid operation, operand 1 is infinite collection
      """
      And got the following notation:
      """
      list= .list2
          ^
      """
      When use a instance of java class "Container" to evaluate:
      """
      list2= .list
      """
      Then failed with the message:
      """
      Invalid operation, operand 2 is infinite collection
      """
      And got the following notation:
      """
      list2= .list
           ^
      """

  Rule: object

    Scenario: compare other object
      Then the following verification should pass:
      """
      1= 1
      """
      When evaluate by:
      """
      1= 2
      """
      Then failed with the message:
      """
      Expected to be equal to: java.lang.Integer
      <2>
       ^
      Actual: java.lang.Integer
      <1>
       ^
      """
      And got the following notation:
      """
      1= 2
         ^
      """

    Scenario: with input
      Given the following json:
      """
      2
      """
      Then the following verification should pass:
      """
      = 2
      """
      When evaluate by:
      """
      = 1
      """
      Then failed with the message:
      """
      Expected to be equal to: java.lang.Integer
      <1>
       ^
      Actual: java.lang.Integer
      <2>
       ^
      """
      And got the following notation:
      """
      = 1
        ^
      """
