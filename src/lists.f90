!example of parametric list usage
#define STRING32 32
module str_lists
#define TYPEPARAM character(STRING32)
#include "list-inc-def.f90"
contains
#include "list-inc-proc.f90"
#undef TYPEPARAM
end module

module gen_lists
  !pointer variant of the generic list.

  implicit none
  
  type nil
  end type

  type list_node
    class(*),pointer :: item
    type(list_node),pointer :: next =>null()
  end type list_node

  type list
    type(list_node),pointer,private :: first => null()
    type(list_node),pointer,private :: last => null()
    type(list_node),pointer,private :: iter => null()
    integer                ,private :: length = 0
    contains
      procedure :: finalize => list_finalize
      procedure :: add => list_add
!       procedure :: move_alloc => list_move_alloc
      procedure :: iter_next => list_iter_next
      procedure :: iter_restart => list__iter_restart
      procedure :: for_each => list_for_each
      procedure :: all => list_All
      procedure :: any => list_Any
      procedure :: len => list_len
      procedure :: get_first => list_get_first
      procedure :: get_last => list_get_last
      procedure :: get_nth => list_get_nth
      procedure :: push => list_push
      procedure :: pop => list_pop
      procedure :: pop_first => list_pop_first
  end type list

  abstract interface
    subroutine foreach_sub(item)
      import
      class(*) :: item
    end subroutine
    logical function elem_logical_fun(item)
      import
      class(*),intent(in) :: item
    end function
  end interface


contains

  recursive subroutine list_finalize(self)
    class(list),intent(inout) :: self
    type(list_node),pointer :: node,tmp

    node => self%first

    do while (associated(node))
      tmp => node
      node => node%next
      
      select type (it=>tmp%item)
      !NOTE: finalization would be better, 
      !  but not supported by gfortran 4.8
        class is (list)
          call it%finalize()
      end select
      
      deallocate(tmp%item)
      deallocate(tmp)
    end do

    self%last => null()
    self%first => null()

    self%length = 0
    

  end subroutine



  subroutine list_add(self,item)
    class(list),intent(inout) :: self
    class(*),intent(in) :: item

    if (.not.associated(self%last)) then
      allocate(self%first)
      self%last => self%first
    else
      allocate(self%last%next)
      self%last => self%last%next
    endif

    allocate(self%last%item, source=item)

    self%length = self%length + 1

  end subroutine


!     subroutine list_move_alloc(self,item)
!       class(list),intent(inout) :: self
!       class(*),intent(inout),allocatable :: item
! 
!       if (.not.associated(self%last)) then
!         allocate(self%first)
!         self%last => self%first
!       else
!         allocate(self%last%next)
!         self%last => self%last%next
!       endif
! 
!       call move_alloc(item, self%last%item)
! 
!       self%length = self%length + 1
! 
!     end subroutine


  subroutine list__iter_restart(self)
    class(list),intent(inout) :: self

    self%iter => self%first

  end subroutine


  subroutine list_iter_next(self,res)
    class(list),intent(inout) :: self
    class(*),pointer,intent(out) :: res

    if (associated(self%iter)) then
      res => self%iter%item
      self%iter => self%iter%next
    else
      res => null()
    end if
  end subroutine

  subroutine list_for_each(self,proc)
    class(list) :: self
    procedure(foreach_sub) :: proc
    type(list_node),pointer :: node

    node => self%first

    do while (associated(node))
      if (associated(node%item)) call proc(node%item)
      node => node%next
    end do

  end subroutine

  logical function list_all(self,proc) result(res)
    class(list),intent(inout) :: self
    procedure(elem_logical_fun) :: proc
    type(list_node),pointer :: node

    res = .true.
    
    node => self%first

    do while (associated(node))
      if (associated(node%item)) then
        res =  proc(node%item)
        if (.not.res) return
      end if
      node => node%next
    end do

  end function

  logical function list_any(self,proc) result(res)
    class(list),intent(inout) :: self
    procedure(elem_logical_fun) :: proc
    type(list_node),pointer :: node

    res = .false.
    
    node => self%first

    do while (associated(node))
      if (associated(node%item)) then
        res =  proc(node%item)
        if (res) return
      end if
      node => node%next
    end do

  end function

  pure integer function list_len(self)
    class(list),intent(in) :: self

    list_len = self%length
  end function
  
  function list_get_last(self) result(res)
    class(*),pointer :: res
    class(list),intent(in) :: self

    res => self%last%item
  end function
  
  function list_get_first(self) result(res)
    class(*),pointer :: res
    class(list),intent(in) :: self

    res => self%first%item
  end function
  
  function list_get_nth(self,n) result(res)
    class(*),pointer :: res
    class(list),intent(in) :: self
    integer, intent(in) :: n
    integer :: i
    type(list_node),pointer :: node

    if (associated(self%first).and.self%length>=n) then
      node => self%first
      i = 1
      do
        if (i==n) then
          res => node%item
          exit
        else if (i<n.and..not.associated(node%next)) then
          res => null()
          exit
        else
          i = i + 1
          node => node%next
        end if
      end do
    else
      res => null()
    end if
  end function
  
  
  subroutine list_pop(self, item)
    class(list),intent(inout) :: self
    class(*),pointer,intent(out) :: item
    type(list_node),pointer :: node, previous

    if (associated(self%last)) then
      item => self%last%item
      
      previous => null()
      node => self%first
      
      do while (associated(node%next))
        previous => node
        node => node%next
      end do
      
      if (associated(previous)) then
        deallocate(previous%next)
      else
       deallocate(self%first)
      end if
      
      self%last => previous
      
      self%length = self%length - 1
    else
      item => null()
    end if
  end subroutine
  
  subroutine list_push(self, item)
    class(list),intent(inout) :: self
    class(*),pointer,intent(in) :: item

    call self%add(item)
  end subroutine
  
  subroutine list_pop_first(self, item)
    class(list),intent(inout) :: self
    class(*),pointer,intent(out) :: item
    type(list_node),pointer :: node
    
    if (associated(self%first)) then
      item => self%first%item
      
      node => self%first%next
      deallocate(self%first)
      self%first => node
      
      self%length = self%length - 1
      if (self%length==0) nullify(self%last)
    else
      item => null()
    end if
  end subroutine
    
end module

module lists
  use str_lists, only: str_list => list
  use gen_lists, only: list, nil
end module lists



