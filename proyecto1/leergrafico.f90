program leer_grafico
    implicit none

    type Pais
        character(len=100) :: nombre
        integer :: poblacion
        integer :: saturacion
        character(len=256) :: bandera
    end type Pais

    type Continente
        character(len=100) :: nombre
        type(Pais), dimension(:), allocatable :: paises
    end type Continente

    type Grafica
        character(len=100) :: nombre
        type(Continente), dimension(:), allocatable :: continentes
    end type Grafica

    ! Variables para almacenar la gráfica
    type(Grafica) :: grafico
    type(Pais), dimension(:),allocatable :: lista_paises
    type(Continente), allocatable :: lista_cont(:)
    integer :: i,j, num_continentes, num_paises,ios,count,valorS
    character(len=256) ::  clave, valor,charPor
    logical :: contieneCont,contieneGrafica,contienePais
    
    character(len=256) :: linea, archivo
    contieneGrafica = .false.
    contieneCont = .false.
    contienePais = .false.
    num_continentes = 0
    num_paises=0
    count=0
    ! Pedir el nombre del archivo al usuario
    print *, 'Ingrese el nombre del archivo que desea leer (por ejemplo, grafica.txt): '
    read *, archivo
  
    ! Abrir el archivo para lectura
    open(unit=10, file=trim(archivo), status='old', action='read', iostat=ios)
 
    allocate(lista_paises(1))
    allocate(lista_cont(1)) 
    if (ios /= 0) then
        print *, "Error al abrir el archivo: ", trim(archivo)
        stop
    endif
  
    ! Leer y procesar cada línea
    do while (.true.)
        read(10, '(A)', iostat=ios) linea
        if (ios /= 0) exit  ! Salir del bucle si hay un error o se llega al final del archivo
        
        if (index(linea, 'grafica') > 0) then
            contieneGrafica = .true.
            contieneCont = .false.
            contienePais = .false.
        elseif (index(linea, 'continente') > 0) then
            contieneCont = .true.
            contienePais = .false.
            num_continentes = num_continentes + 1
            call agregar_continente(lista_cont, num_continentes)  ! Agregar nuevo continente
            print *, 'Abre continente'
    
        elseif (index(linea, 'pais') > 0) then
            contienePais = .true.
            print*,'abre pais '
            num_paises = num_paises + 1
            call agregar_pais(lista_paises, num_paises)
        elseif (index(linea, 'nombre') > 0) then
            if (contieneGrafica .and. .not. contieneCont .and. .not. contienePais) then
                print *, 'se encontro el nombre del gráfico'
            else if(contieneGrafica .and. contieneCont .and. .not. contienePais)then

                print *, 'se encontro nombre del contienente '
                lista_cont(num_continentes)%nombre = trim(extraer_valor(linea))
            else if (contieneGrafica .and. contieneCont .and. contienePais) then 
                print *, 'se encontro nombre del pais '
                print*,'nombre ', trim(extraer_valor(linea))
                lista_paises(num_paises)%nombre= trim(extraer_valor(linea))

            end if
        elseif(index(linea,'}')>0)then 
            if (contieneGrafica .and. contieneCont .and. contienePais) then
                print *, 'se cierra pais'
                contienePais = .false.
            elseif (contieneGrafica .and. contieneCont .and. .not. contienePais) then
                print *, 'se cierra continente'
                lista_cont(num_continentes)%paises = lista_paises  ! Asignar la lista de países al continente
            contieneCont = .false.
            num_paises = 0  ! Reiniciar la cuenta de países para el próximo continente
            deallocate(lista_paises)  ! Liberar la lista actual de países
            allocate(lista_paises(1))  ! Reasignar espacio para el próximo continente
                contieneCont = .false.
            elseif (contieneGrafica .and. .not. contieneCont .and. .not. contienePais) then
                print *, 'se cierra grafica'
                contieneGrafica = .false.
            end if
       
        elseif(index(linea,'poblacion')>0)then
                 if (contieneGrafica .and. contieneCont .and. contienePais) then 
                    print *, 'se encontro poblacion '
                    print*,'poblacion ', trim(extraer_valor(linea))
                    valor=trim(extraer_valor(linea))
                    READ(valor, *) lista_paises(num_paises)%poblacion
                 end if
         elseif(index(linea,'saturacion')>0)then
                if (contieneGrafica .and. contieneCont .and. contienePais) then 
                    print *, 'se encontro saturacion '
                    print*,'poblacion ', trim(extraer_valor(linea))
                    charPor=trim(extraer_valor(linea))
                    valorS=(extraer_porcentaje(charPor))
                    print*,'sin %',valorS
                    lista_paises(num_paises)%saturacion = valorS
                end if
        elseif(index(linea,'bandera')>0)then
            if (contieneGrafica .and. contieneCont .and. contienePais) then 
                        print *, 'se encontro bandera '
                        lista_paises(num_paises)%bandera= trim(extraer_valor(linea))
            end if
        end if

        ! Procesar la línea aquí
        print *, trim(linea)
    end do
  
    ! Cerrar el archivo
    close(10)
    ! Inicializar banderas de control
    do i = 1, size(lista_paises)
        print *, 'Pais ', i, ':'
        print *, '  Nombre: ', trim(lista_paises(i)%nombre)
        
        print *, ' '
    end do
   
    do i = 1, size(lista_cont)
        print *, "Continente: ", lista_cont(i)%nombre
        print *, "Paises: "
        do j = 1, size(lista_cont(i)%paises)
            print *, "  ", lista_cont(i)%paises(j)%nombre ! assuming Pais has a nombre component
        end do
    end do
  contains 
  


  function extraer_valor(linea) result(valor)
    implicit none
    character(len=*), intent(in) :: linea
    character(len=256) :: valor
    integer :: pos_inicio, pos_fin

    ! Inicializar el valor
    valor = ""

    ! Buscamos si el valor es un texto entre comillas
    pos_inicio = index(linea, '"')
    if (pos_inicio > 0) then
        pos_fin = index(linea(pos_inicio+1:), '"') + pos_inicio
        if (pos_inicio > 0 .and. pos_fin > pos_inicio) then
            valor = linea(pos_inicio+1:pos_fin-1)
            return
        end if
    end if

    ! Buscamos si el valor es numérico (después de ':')
    pos_inicio = index(linea, ':')
    pos_fin = index(linea, ';')
    if (pos_inicio > 0 .and. pos_fin > pos_inicio) then
        valor = linea(pos_inicio+1:pos_fin-1)
    end if

end function extraer_valor
subroutine agregar_pais(lista_paises, num_paises)
    implicit none
    type(Pais), allocatable, intent(inout) :: lista_paises(:)
    integer, intent(in) :: num_paises
    type(Pais), allocatable :: lista_paises_temp(:)

    ! Redimensionamos la lista de países
    allocate(lista_paises_temp(size(lista_paises)))
    lista_paises_temp = lista_paises

    ! Expandimos la lista original
    deallocate(lista_paises)
    allocate(lista_paises(num_paises))

    ! Copiamos los datos antiguos de vuelta
    lista_paises(1:num_paises-1) = lista_paises_temp

    ! Desasignar temporal
    deallocate(lista_paises_temp)
end subroutine agregar_pais
function extraer_porcentaje(cadena) result(valor)
    character(len=*), intent(in) :: cadena
    integer :: valor
    character(len=10) :: temp
    integer :: pos

    ! Eliminar el símbolo de porcentaje
    pos = index(cadena, '%')
    if (pos > 0) then
        temp = cadena(1:pos-1)
    else
        temp = cadena
    endif

    ! Convertir la cadena a entero
    read(temp, *) valor
  end function extraer_porcentaje
  subroutine agregar_continente(lista_cont, num_continentes)
    implicit none
    type(Continente), allocatable, intent(inout) :: lista_cont(:)
    integer, intent(in) :: num_continentes
    type(Continente), allocatable :: lista_cont_temp(:)

    ! Redimensionamos la lista de continentes
    allocate(lista_cont_temp(size(lista_cont)))
    lista_cont_temp = lista_cont

    ! Expandimos la lista original
    deallocate(lista_cont)
    allocate(lista_cont(num_continentes))

    ! Copiamos los datos antiguos de vuelta
    lista_cont(1:num_continentes-1) = lista_cont_temp

    ! Desasignar temporal
    deallocate(lista_cont_temp)
end subroutine agregar_continente

end program leer_grafico