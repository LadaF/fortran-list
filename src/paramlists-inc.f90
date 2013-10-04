
  implicit none
  
!   type nil
!   end type

  type list_node
    TYPEPARAM :: item
    type(list_node),pointer :: next =>null()
  end type list_node

  type list
    type(list_node),pointer,private :: first => null()
    type(list_node),pointer,private :: last => null()
    type(list_node),pointer,private :: iter => null()
    integer                ,private :: length = 0
    contains
      procedure :: deallocate => list_deallocate
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
    subroutine for_each_sub(item)
      import
      TYPEPARAM,intent(inout) :: item
    end subroutine
    logical function elem_logical_fun(item)
      import
      TYPEPARAM,intent(in) :: item
    end function
  end interface


  contains


    subroutine list_deallocate(self)
      class(list),intent(inout) :: self
      type(list_node),pointer :: node,tmp

      node => self%first

      do while (associated(node))
        tmp => node
        node => node%next

        deallocate(tmp)
      end do

      self%last => null()
      self%first => null()

      self%length = 0
      

    end subroutine



    subroutine list_add(self,item)
      class(list),intent(inout) :: self
      TYPEPARAM,intent(in) :: item

      if (.not.associated(self%last)) then
        allocate(self%first)
        self%last => self%first
      else
        allocate(self%last%next)
        self%last => self%last%next
      endif

      self%last%item = item

      self%length = self%length + 1

    end subroutine


    subroutine list__iter_restart(self)
      class(list),intent(inout) :: self

      self%iter => self%first

    end subroutine


    subroutine list_iter_next(self,res)
      class(list),intent(inout) :: self
      TYPEPARAM,intent(out) :: res

      if (associated(self%iter)) then
        res = self%iter%item
        self%iter => self%iter%next
      else
        res = ''
      end if
    end subroutine

    subroutine list_for_each(self,proc)
      class(list),intent(inout) :: self
      procedure(for_each_sub) :: proc
      type(list_node),pointer :: node

      node => self%first

      do while (associated(node))
        call proc(node%item)
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
        res =  proc(node%item)
        if (.not.res) return
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
        res =  proc(node%item)
        if (res) return
        node => node%next
      end do

    end function

    pure integer function list_len(self)
      class(list),intent(in) :: self

      list_len = self%length
    end function
    
    function list_get_last(self) result(res)
      TYPEPARAM :: res
      class(list),intent(in) :: self

      if (associated(self%last)) res = self%last%item
    end function
    
    function list_get_first(self) result(res)
      TYPEPARAM :: res
      class(list),intent(in) :: self

      if (associated(self%first)) res = self%first%item
    end function
    
    subroutine list_pop(self, item)
      class(list),intent(inout) :: self
      TYPEPARAM,intent(out) :: item
      type(list_node),pointer :: node, previous

      if (associated(self%last)) then
        item=self%last%item
        
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

      end if
    end subroutine
    
    subroutine list_push(self, item)
      class(list),intent(inout) :: self
      TYPEPARAM,intent(in) :: item

      call self%add(item)
    end subroutine
    
    subroutine list_pop_first(self, item)
      class(list),intent(inout) :: self
      TYPEPARAM,intent(out) :: item
      type(list_node),pointer :: node
      
      if (associated(self%first)) then
        item=self%first%item
        
        node => self%first%next
        deallocate(self%first)
        self%first => node
        
        self%length = self%length - 1
        if (self%length==0) nullify(self%last)

      end if
    end subroutine