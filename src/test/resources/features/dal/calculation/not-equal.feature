Feature: not-equal

  Rule: null

    Scenario: compare primitive null
      Then the following verification should pass:
      """
      1 != null
      """
      And the following verification should pass:
      """
      null != 1
      """
      When evaluate by:
      """
      null != null
      """
      Then the result should:
      """
      = false
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
      When use a instance of java class "BeanContainer" to evaluate:
      """
      bean1 != .bean2
      """
      Then the result should:
      """
      = false
      """
      When use a instance of java class "BeanContainer" to evaluate:
      """
      null != .bean2
      """
      Then the result should:
      """
      = false
      """
      When use a instance of java class "BeanContainer" to evaluate:
      """
      bean1 != null
      """
      Then the result should:
      """
      = false
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
        "list": [2],
        "empty": [2],
        "list1": [1]
      }
      """
      Then the following verification should pass:
      """
      list != .empty
      """
      Then the result should:
      """
      = false
      """
      When evaluate by:
      """
      list != .list1
      """
      Then the result should:
      """
      = true
      """

    Scenario: compare customized list
      Given the following java class:
      """
      public class Container {
        public ListObject list1 = new ListObject();
        public ListObject list2 = new ListObject();
        public ListObject list3 = new ListObject();
        public int[] list4 = new int[] {0, 2, 4, 6, 8};
        public ListObject list5 = new ListObject();
        public int[] list6 = new int[] {};
      }
      """
      When use a instance of java class "Container" to evaluate:
      """
      list1 != .list2
      """
      Then the result should:
      """
      = false
      """
      When use a instance of java class "Container" to evaluate:
      """
      list3 != .list4
      """
      Then the result should:
      """
      = false
      """
      When use a instance of java class "Container" to evaluate:
      """
      list5 != .list6
      """
      Then the result should:
      """
      = true
      """

    Scenario: use object compare when another object is not list
      Given the following java class:
      """
      public class Container {
        public ListObject list1 = new ListObject();
        public ListObject list2 = new ListObject() {
          public boolean equals(Object obj) {
              return true;
          }
        };
      }
      """
      When use a instance of java class "Container" to evaluate:
      """
      list1 != 'not-list'
      """
      Then the result should:
      """
      = true
      """
      When use a instance of java class "Container" to evaluate:
      """
      list2 != 'any-object'
      """
      Then the result should:
      """
      = false
      """

  Rule: object

    Scenario: compare other object
      When evaluate by:
      """
      1 != 1
      """
      Then the result should:
      """
      = false
      """
      When evaluate by:
      """
      1 != 2
      """
      Then the result should:
      """
      = true
      """

    Scenario: with input
      Given the following json:
      """
      2
      """
      When evaluate by:
      """
      != 2
      """
      Then the result should:
      """
      = false
      """
      When evaluate by:
      """
      != 1
      """
      Then the result should:
      """
      = true
      """
