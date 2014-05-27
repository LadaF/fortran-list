
  type list_node
    TYPEPARAM :: item
    type(list_node),pointer :: next =>null()
  end type list_node

#ifdef FINALIZABLE
  type, extends(finalizable) :: list
#else
  type list
#endif
    type(list_node),pointer,private :: first => null()
    type(list_node),pointer,private :: last => null()
    type(list_node),pointer,private :: iter => null()
    integer                ,private :: length = 0
    contains
      procedure :: finalize => list_finalize
      procedure :: add => list_add
      procedure :: iter_next => list_iter_next
      procedure :: iter_restart => list__iter_restart
      procedure :: for_each => list_for_each
      procedure :: all => list_All
      procedure :: any => list_Any
      procedure :: len => list_len
      procedure :: get_first => list_get_first
      procedure :: get_last => list_get_last
      procedure :: push => list_push
      procedure :: pop => list_pop
      procedure :: pop_first => list_pop_first
  end type list

  abstract interface
    subroutine foreach_sub(item)
      import
      TYPEPARAM :: item
    end subroutine
    logical function elem_logical_fun(item)
      import
      TYPEPARAM,intent(in) :: item
    end function
  end interface

