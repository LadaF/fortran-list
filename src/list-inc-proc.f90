


    recursive subroutine list_finalize(self)
      class(list),intent(inout) :: self
      type(list_node),pointer :: node,tmp

      node => self%first

      do while (associated(node))
        tmp => node
        node => node%next
#ifdef FINALIZABLE
        call tmp%item%finalize
#endif
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


    function list_iter_next(self) result(res)
      class(list),intent(inout) :: self
      TYPEPARAM,pointer :: res

      if (associated(self%iter)) then
        res => self%iter%item
        self%iter => self%iter%next
      else
        res => null()
      end if
    end function

    recursive subroutine list_for_each(self,proc)
      class(list),intent(inout) :: self
      procedure(foreach_sub) :: proc
      type(list_node),pointer :: node

      node => self%first

      do while (associated(node))
        call proc(node%item)
        node => node%next
      end do

    end subroutine

    recursive logical function list_all(self,proc) result(res)
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

    recursive logical function list_any(self,proc) result(res)
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