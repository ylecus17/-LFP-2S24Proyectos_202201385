program main
    implicit none
    integer :: i,j, len, linea, columna, estado, puntero, numErrores, numTokens, file_unit, ios, content_size
!============================objeto token----------------------------------------------
    type :: TokenInfo
    character(len=200) :: lexema  ! Lexema (token)
    character(len=100) :: descripcion  ! Descripción o tipo del token
    integer :: columna      ! Columna donde ocurrió el token
    integer :: linea        ! Línea donde ocurrió el token
end type TokenInfo
!============================objeto de las listas===============================
    type Pais
        character(len=100) :: nombre
        integer :: poblacion
        integer:: saturacion
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

!============================variables de estado------------------------------------------

    character(len=1) :: char 
    character(len=100) :: tkn,nombreGrafica
    character(len=1), dimension(26) :: A 
    character(len=1), dimension(26) :: M
    character(len=1), dimension(4) :: S 
    character(len=1), dimension(11) :: N
    character(len=10), dimension(7) :: palabrasReservadas
    character(len=500) :: buffer
    character(len=:), allocatable :: contenido
    type(Grafica) :: grafico
    type(Pais), dimension(:),allocatable :: lista_paises
    type(Continente), allocatable :: lista_cont(:)
    
    character(len=500) ::   valor,charPor
    
    character(len=500) :: line
    A = ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z']
    M = ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']
    S = [':','{','}',';']
    N=['0','1','2','3','4','5','6','7','8','9','%']
    palabrasReservadas = ['grafica   ', 'nombre    ','continente','pais      ','bandera   ','poblacion ','saturacion']  ! Palabras reservadas
    
!================ variable de las listas========================    
    
    
    
  
    contenido = ""
    content_size = 0

    print *, 'Iniciando la lectura del contenido...'

    ! Leer la entrada desde la consola (suponiendo que es desde Tkinter)
    do
        read(*, '(A)', IOSTAT=ios) buffer
        if (ios /= 0) exit  ! Termina cuando ya no hay más líneas

        ! Calculamos el tamaño de la línea y actualizamos el tamaño total
        content_size = content_size + len_trim(buffer) + 1  ! +1 para el salto de línea

        ! Si el buffer 'contenido' ya está asignado, lo extendemos
        if (allocated(contenido)) then
            contenido = contenido // trim(buffer) // new_line('a')
        else
            allocate(character(len=content_size) :: contenido)
            contenido = trim(buffer) // new_line('a')
        endif

        print *, 'Se leyó una línea del contenido.'
    end do

    

    nombreGrafica=""
    estado = 0
    puntero = 1
    columna = 1
    linea = 1
    numErrores = 0
    numTokens = 0
    tkn = ""
    ! ---- Análisis Léxico ----
    ! Finaliza la lectura del contenido
    print *, 'Contenido completo leído:'
    

    ! Aquí llamas al analizador léxico con el contenido leído
    call analizador_lexico(contenido)
    contains   


    subroutine analizador_lexico(contenido)
        implicit none 
        
        character(len=:), allocatable :: contenido
        
        type(TokenInfo), dimension(500) :: tokens, errores
        print*, 'llego a analisis'
        print*, contenido
        len = len_trim(contenido)
            ! estados 
        do while (puntero <= len)
            char = contenido(puntero:puntero)
            
            ! Control de nueva línea, tabulación y espacio
            if (ichar(char) == 10) then
                columna = 1
                linea = linea + 1
                puntero = puntero + 1
            elseif (ichar(char) == 9) then
                columna = columna + 4
                puntero = puntero + 1
            elseif (ichar(char) == 32) then
                columna = columna + 1
                puntero = puntero + 1
            else
                select case (estado)
                    case (0) ! Estado inicial, chequeo de minúscula
                        if (any(char == M)) then
                            estado = 1
                        else
                            numErrores = numErrores + 1
                            errores(numErrores) = TokenInfo(char, "Se esperaba una minúscula", columna, linea)
                            puntero = puntero + 1
                        end if
                    case (1) ! Estado de lectura de minúsculas
                        if (any(char == M)) then
                            tkn = trim(tkn) // char
                            columna = columna + 1
                            puntero = puntero + 1
                        elseif (any(char == S)) then
                            ! Estado de transición, se encuentra un símbolo
                            if (es_palabra_reservada(tkn)) then
                                numTokens = numTokens + 1
                                tokens(numTokens) = TokenInfo(trim(tkn), "Palabra reservada", columna, linea)
                            else
                                numErrores = numErrores + 1
                                errores(numErrores) = TokenInfo(trim(tkn), "No es una palabra reservada", columna, linea)
                            end if
                            tkn = ""
                            estado = 2
                        else
                            numErrores = numErrores + 1
                            errores(numErrores) = TokenInfo(char, "Caracter inesperado", columna, linea)
                            puntero = puntero + 1
                        end if
                    case (2) ! Estado de procesamiento de símbolos
                        if (char == '"') then
                            ! Procesar contenido dentro de comillas
                            estado = 3
                            puntero = puntero + 1
                        elseif (any(char == S)) then
                            ! Si es un símbolo permitido, guardarlo
                            numTokens = numTokens + 1
                            tokens(numTokens) = TokenInfo(char, "Signo", columna, linea)
                            columna = columna + 1
                            puntero = puntero + 1
                        elseif (any(char == M)) then
                            ! Si es una letra después del signo, volver al estado 1
                            estado = 1
                        else
                            ! Nuevo estado para leer dígitos después de un símbolo
                            estado = 4
                            tkn = tkn // char
                            columna = columna + 1
                            puntero = puntero + 1
                        end if
                    case (3) ! Estado de ignorar contenido dentro de comillas
                        if (char == '"') then
                            ! Termina la lectura de la cadena, volver al estado de símbolos
                            estado = 2
                        end if
                        ! Aumentar columna durante la lectura de las comillas
                        columna = columna + 1
                        puntero = puntero + 1
                    case (4) ! Nuevo estado para leer dígitos después de un símbolo
                        if (any(char == N)) then
                            tkn = tkn // char
                            columna = columna + 1
                            puntero = puntero + 1
                        else
                            ! Termina la lectura de los dígitos, volver al estado de símbolos
                            estado = 2
                        end if
                end select
            end if
        end do

    if (numErrores > 0) then
        call generar_html_errores(numErrores, errores)
        call generar_html_tokens(numTokens, tokens)
        print*,'se genero errores'
    else
        print*,' no hay errores se mando a listas'
        call generar_html_tokens(numTokens, tokens)
        call generar_html_errores(numErrores, errores)
        call guardarListas(contenido,lista_paises,lista_cont)

    end if
print*, 'analizado'
end subroutine analizador_lexico
subroutine guardarListas(contenido, lista_paises, lista_cont)
    implicit none
    logical :: contieneGrafica, contieneCont, contienePais
    character(len=100) :: valor, charPor,nombreGrafica
    
    integer :: num_continentes, num_paises, pos_inicio, pos_fin, len_contenido, ios,valorS
    type(Pais), dimension(:),allocatable :: lista_paises
    type(Continente), allocatable :: lista_cont(:)
    character(len=*), intent(in) :: contenido
    character(len=256) :: line
    type(Pais), dimension(:), allocatable :: temp_lista_paises

    contieneGrafica = .false.
    contieneCont = .false.
    contienePais = .false.
    num_continentes = 0
    num_paises = 0
    allocate(lista_paises(1))
    allocate(lista_cont(1))
    len_contenido = len_trim(contenido)
    
    ! Inicializar posición
    pos_inicio = 1

    do while (pos_inicio <= len_contenido)
        ! Encontrar la posición del siguiente salto de línea
        pos_fin = index(contenido(pos_inicio:), new_line('a'))

        if (pos_fin == 0) then
            ! Si no hay más saltos de línea, toma el resto del contenido
            line = trim(contenido(pos_inicio:))
            pos_inicio = len_contenido + 1
        else
            ! Extraer una línea del contenido
            line = trim(contenido(pos_inicio:pos_inicio + pos_fin - 2))
            pos_inicio = pos_inicio + pos_fin
        end if

        ! Procesar la línea
        if (index(line, 'grafica') > 0) then
            contieneGrafica = .true.
            contieneCont = .false.
            contienePais = .false.
        elseif (index(line, 'continente') > 0) then
            contieneCont = .true.
            contienePais = .false.
            num_continentes = num_continentes + 1
            call agregar_continente(lista_cont, num_continentes)  ! Agregar nuevo continente
            print *, 'Abre continente'

            

        elseif (index(line, 'pais') > 0) then
            contienePais = .true.
            print*,'abre pais '
            num_paises = num_paises + 1
            call agregar_pais(lista_paises, num_paises)
        elseif (index(line, 'nombre') > 0) then
            if (contieneGrafica .and. .not. contieneCont .and. .not. contienePais) then
                print *, 'Se encontró el nombre del gráfico'
                nombreGrafica=trim(extraer_valor(line))
            elseif (contieneGrafica .and. contieneCont .and. .not. contienePais) then
                print *, 'Se encontró nombre del continente'
                lista_cont(num_continentes)%nombre = trim(extraer_valor(line))
            elseif (contieneGrafica .and. contieneCont .and. contienePais) then
                print *, 'Se encontró nombre del país'
                lista_paises(num_paises)%nombre = trim(extraer_valor(line))
            end if
        elseif (index(line, '}') > 0) then
            if (contieneGrafica .and. contieneCont .and. contienePais) then
                print *, 'Se cierra país'
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
                print *, 'Se cierra gráfica'
                contieneGrafica = .false.
            end if
        elseif (index(line, 'poblacion') > 0) then
            if (contieneGrafica .and. contieneCont .and. contienePais) then
                print *, 'Se encontró población'
                valor = trim(extraer_valor(line))
                read(valor, *) lista_paises(num_paises)%poblacion
            end if
        elseif (index(line, 'saturacion') > 0) then
            if (contieneGrafica .and. contieneCont .and. contienePais) then
                print *, 'Se encontró saturación'
                charPor = trim(extraer_valor(line))
                valorS = extraer_porcentaje(charPor) 
                lista_paises(num_paises)%saturacion = valorS
            end if
        elseif (index(line, 'bandera') > 0) then
            if (contieneGrafica .and. contieneCont .and. contienePais) then
                print *, 'Se encontró bandera'
                lista_paises(num_paises)%bandera = trim(extraer_valor(line))
            end if
        end if

        print *, trim(line)
    end do
    
   
    do i = 1, size(lista_cont)
        print *, "Continente: ", lista_cont(i)%nombre
        print *, "Paises: "
        do j = 1, size(lista_cont(i)%paises)
            print *, "  ", lista_cont(i)%paises(j)%nombre ! assuming Pais has a nombre component
        end do
    end do
    call generarDot(lista_cont,num_continentes,nombreGrafica)
end subroutine guardarListas






subroutine generarDOT(lista_cont, num_continentes, nombreGrafica)
    implicit none
    ! Declaración de parámetros de entrada
    type(Continente), intent(in) :: lista_cont(:)  ! Lista de continentes
    integer, intent(in) :: num_continentes  ! Número de continentes
    character(len=*), intent(in) :: nombreGrafica  ! Nombre de la gráfica
    
    ! Variables locales
    integer :: i, j, num_paises
    character(len=500) :: dot_line
    integer::saturacion_continente,saturacion_pais
    
    character(len=20) :: color_continente, color_pais
    character(len=10) :: poblacion_str, saturacion_str
    character(len=50) :: id_continente, id_pais
    
    ! Abrir archivo DOT para escribir
    open(unit=10, file="grafica.dot", status="replace", action="write")
    
    ! Escribir encabezado DOT
    write(10, *) "digraph G {"
    write(10, *) "rankdir=LR;"  ! Dirección de la jerarquía de arriba a abajo
    
    ! Primer nivel: Nombre de la gráfica (usamos "G" como ID pero mostramos el nombre visible correctamente)
    write(10, *) "G [label="""//trim(adjustl(nombreGrafica))//""", shape=box, style=filled, fillcolor=lightblue];"
    
    ! Segundo nivel: Continentes
    do i = 1, num_continentes
        ! Reemplazar espacios en el ID del continente con guiones bajos
        id_continente = trim(adjustl(lista_cont(i)%nombre))
        call reemplazar_espacios(id_continente, "_")
        
        ! Obtener la saturación promedio del continente
        saturacion_continente = obtener_saturacion_promedio(lista_cont(i))
        color_continente = obtener_color(saturacion_continente)
        
        ! Escribir el nodo del continente
        write(10, '(A,A,A,A)') id_continente, &  ! Usamos el ID con guiones bajos
        " [label=""", trim(adjustl(lista_cont(i)%nombre)), &  ! El nombre visible con espacios
        """, style=filled, fillcolor=", trim(color_continente), "];"
        
        ! Conectar gráfico con el continente
        write(10, *) "G -> ", id_continente, ";"
        
        ! Obtener el número de países en el continente
        num_paises = size(lista_cont(i)%paises)
        
        ! Tercer nivel: Países dentro de cada continente
        do j = 1, num_paises
            saturacion_pais = lista_cont(i)%paises(j)%saturacion
            color_pais = obtener_color(saturacion_pais)
            
            ! Reemplazar espacios en el ID del país
            id_pais = trim(adjustl(lista_cont(i)%paises(j)%nombre))
            call reemplazar_espacios(id_pais, "_")
            
            ! Convertir los valores de población y saturación a string
            write(poblacion_str, '(I10)') lista_cont(i)%paises(j)%poblacion
            write(saturacion_str, '(I10)') lista_cont(i)%paises(j)%saturacion

            ! Escribir el nodo del país
            write(10, '(A, A, A, A, A, A, A)') id_pais, & 
            " [label=""Nombre: ", trim(lista_cont(i)%paises(j)%nombre), & 
            "\nPoblación: ", trim(poblacion_str), & 
            "\nSaturación: ", trim(saturacion_str), "%""", & 
            ", style=filled, fillcolor=", trim(color_pais), "];"
            
            ! Conectar continente con país
            write(10, *) id_continente, " -> ", id_pais, ";"
        end do
    end do
    
    ! Cerrar el archivo DOT
    write(10, *) "}"
    close(10)
end subroutine generarDOT

! Subroutine to replace spaces with underscores
subroutine reemplazar_espacios(str, reemplazo)
    implicit none
    character(len=*), intent(inout) :: str
    character(len=*), intent(in) :: reemplazo
    integer :: i
    do i = 1, len_trim(str)
        if (str(i:i) == ' ') str(i:i) = reemplazo
    end do
end subroutine reemplazar_espacios

!===============funciones auxiliares analizador lexico=====================
    logical function es_palabra_reservada(tkn)
        implicit none
        character(len=*), intent(in) :: tkn
        integer :: i
        es_palabra_reservada = .false.
        do i = 1, size(palabrasReservadas)
            if (trim(tkn) == trim(palabrasReservadas(i))) then
                es_palabra_reservada = .true.
                exit
            end if
        end do
    end function es_palabra_reservada

    subroutine generar_html_tokens(numTokens, tokens)
        implicit none
        integer, intent(in) :: numTokens
        type(TokenInfo), intent(in) :: tokens(numTokens)
        character(len=100000) :: html_content
        character(len=100) :: str_descripcion, str_columna, str_linea,char_token

        integer :: file_unit, ios, i

        ! Si hay errores, se crea el archivo HTML
       if (numTokens > 0) then
    ! Abrir el archivo para escribir
            open(unit=file_unit, file="tokens.html", status="replace", action="write", iostat=ios)
            if (ios /= 0) then
                print *, "Error al crear el archivo HTML."
            else
                ! Escribir la cabecera del HTML directamente al archivo
                write(file_unit, '(A)') '<!DOCTYPE html>' // new_line('a')
                write(file_unit, '(A)') '<html><head><style>' // new_line('a')
                write(file_unit, '(A)') 'table { font-family: Arial, sans-serif;'
                write(file_unit, '(A)') 'border-collapse: collapse; width: 100%; }' // new_line('a')
                write(file_unit, '(A)') 'td, th { border: 1px solid #dddddd; text-align: left; padding: 8px; }' // new_line('a')
                write(file_unit, '(A)') 'tr:nth-child(even) { background-color: #9ebcdb; }' // new_line('a')
                write(file_unit, '(A)') '</style></head><body><h2>Tabla de Tokens</h2>' // new_line('a')
                write(file_unit, '(A)') '<table><tr><th>Carácter</th><th>Descripcion' 
                write(file_unit, '(A)') '</th><th>Columna</th><th>Línea</th></tr>' // new_line('a')

                ! Bucle para formatear cada código ASCII y cada columna

                ! Bucle para agregar filas a la tabla
                do i = 1, numTokens
                    write(str_descripcion, '(A)') trim(tokens(i)%descripcion)
                    write(str_columna, '(I0)') tokens(i)%columna
                    write(str_linea, '(I0)')  tokens(i)%linea
                    write(char_token, '(A)') trim(tokens(i)%lexema)
         
                    ! Escribir cada fila directamente al archivo

                    write(file_unit, '(A)') '<tr><td>' // char_token // '</td><td>' // trim(str_descripcion) // & 
                    '</td><td>' // trim(str_columna) // '</td><td>'&
                     // trim(str_linea) // '</td></tr>' // new_line('a')
                end do

                ! Cerrar la tabla y el HTML
                write(file_unit, '(A)') '</table></body></html>'
                close(file_unit)
            end if
        else
            print *, "no hay tokens."
        end if
    end subroutine generar_html_tokens
    subroutine generar_html_errores(numErrores, errores)
        implicit none
        integer, intent(in) :: numErrores
        type(TokenInfo), intent(in) :: errores(numErrores)
        character(len=100000) :: html_content
        character(len=100) :: str_descripcion, str_columna, str_linea, char_error
        integer :: file_unit, ios, i
    
        ! Abrir el archivo para escribir, reemplazando el existente si ya lo hubiera
        open(unit=file_unit, file="errores.html", status="replace", action="write", iostat=ios)
        if (ios /= 0) then
            print *, "Error al crear el archivo HTML."
        else
            ! Si hay errores, generar la tabla de errores
            if (numErrores > 0) then
                ! Escribir la cabecera del HTML
                write(file_unit, '(A)') '<!DOCTYPE html>' // new_line('a')
                write(file_unit, '(A)') '<html><head><style>' // new_line('a')
                write(file_unit, '(A)') 'table { font-family: Arial, sans-serif;'
                write(file_unit, '(A)') 'border-collapse: collapse; width: 100%; }' // new_line('a')
                write(file_unit, '(A)') 'td, th { border: 1px solid #dddddd; text-align: left; padding: 8px; }' // new_line('a')
                write(file_unit, '(A)') 'tr:nth-child(even) { background-color: #9ebcdb; }' // new_line('a')
                write(file_unit, '(A)') '</style></head><body><h2>Tabla de Errores</h2>' // new_line('a')
                write(file_unit, '(A)') '<table><tr><th>Carácter</th><th>Descripción</th><th>Columna</th><th>Línea</th></tr>' //&
                 new_line('a')
    
                ! Bucle para agregar filas a la tabla
                do i = 1, numErrores
                    write(str_descripcion, '(A)') trim(errores(i)%descripcion)
                    write(str_columna, '(I0)') errores(i)%columna
                    write(str_linea, '(I0)') errores(i)%linea
                    write(char_error, '(A)') trim(errores(i)%lexema)
                    write(file_unit, '(A)') '<tr><td>' // char_error // '</td><td>' // trim(str_descripcion) // & 
                          '</td><td>' // trim(str_columna) // '</td><td>' // trim(str_linea) // '</td></tr>' // new_line('a')
                end do
    
                ! Cerrar la tabla y el HTML
                write(file_unit, '(A)') '</table></body></html>'
            
            ! Si no hay errores, escribir un mensaje indicando que no hay errores
            else
                write(file_unit, '(A)') '<!DOCTYPE html>' // new_line('a')
                write(file_unit, '(A)') '<html><body><h2>No se encontraron errores en el código.</h2></body></html>' // &
                new_line('a')
            end if
            close(file_unit)
        end if
    end subroutine generar_html_errores
!==============================funciones extras listas ============================
    function extraer_valor(line) result(valor)
        implicit none
        character(len=*), intent(in) :: line
        character(len=256) :: valor
        integer :: pos_inicio, pos_fin
    
        ! Inicializar el valor
        valor = ""
    
        ! Buscamos si el valor es un texto entre comillas
        pos_inicio = index(line, '"')
        if (pos_inicio > 0) then
            pos_fin = index(line(pos_inicio+1:), '"') + pos_inicio
            if (pos_inicio > 0 .and. pos_fin > pos_inicio) then
                valor = line(pos_inicio+1:pos_fin-1)
                return
            end if
        end if
    
        ! Buscamos si el valor es numérico (después de ':')
        pos_inicio = index(line, ':')
        pos_fin = index(line, ';')
        if (pos_inicio > 0 .and. pos_fin > pos_inicio) then
            valor = line(pos_inicio+1:pos_fin-1)
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

    
!============funcion auxiliar colores=======================
    function obtener_color(saturacion) result(color)
        integer, intent(in) :: saturacion
        character(len=20) :: color
        
        if (saturacion <= 0.15) then
            color = "white"
        elseif (saturacion <= 30) then
            color = "blue"
        elseif (saturacion <= 45) then
            color = "green"
        elseif (saturacion <= 60) then
            color = "yellow"
        elseif (saturacion <= 75) then
            color = "orange"
        else
            color = "red"
        end if
    end function
    function obtener_saturacion_promedio(lista_cont) result(saturacionPromedio)
        implicit none
        type(Continente), intent(in) :: lista_cont
        integer :: saturacionPromedio
        real :: saturacionPromedioD
        integer :: i, total_paises
        real:: suma_saturacion
        real::decimal
    
        ! Inicializamos la suma y el contador de países
        suma_saturacion = 0
        total_paises = size(lista_cont%paises)
    
        ! Sumamos las saturaciones de todos los países
        do i = 1, total_paises
            print*,'saturacion original', lista_cont%paises(i)%saturacion
            decimal = real(lista_cont%paises(i)%saturacion) / 100.0
            print*,'decimal', decimal
            suma_saturacion = suma_saturacion + decimal
            print*, suma_saturacion
        end do
    
        ! Calculamos el promedio
        if (total_paises > 0) then
            saturacionPromedioD =(suma_saturacion )/ (total_paises)
            print*, 'prd',saturacionPromedioD
        else
            saturacionPromedioD = 0  ! En caso de no haber países
        end if
        saturacionPromedio = nint(saturacionPromedioD * 100)! No necesitamos multiplicar por 100 aquí
        print*, 'promedio',saturacionPromedio
    end function obtener_saturacion_promedio
end program main