!todo el programa se realizo en un solo archivo para  facilitar la carga y la ejecucion del programa por el hecho de compilar y ejecutar 
!se uso los comentarios  para explicar cada parte del programa y facilitar la lectura y comprension del mismo.
!perdon si es largo o dificil de entender :3 

program main
    
    implicit none
    character(len=:), allocatable :: contenido
    integer :: len, fila, columna, estado, puntero, content_size,ios,linea
    character(len=256) :: buffer, aux_tkn
    character(len=1) :: char
    character(len=10000) :: comentario
    
        
        type :: Contenedor
            character(len=20) :: tipo
            character(len=20) :: id
        end type Contenedor
    
        
        
    
    type :: Tkn
        CHARACTER(LEN = 100) :: lexema
        CHARACTER(LEN = 200) :: tipo 
        integer :: fila
        integer :: columna
    End type Tkn
    type :: TknE
        CHARACTER(LEN = 100) :: ultimo_token
        CHARACTER(LEN = 100) :: token_esperado 
        integer :: fila
        integer:: columna
    End type TknE

    ! Declaración de un arreglo de Err para almacenar los errores
    type(TknE), ALLOCATABLE ::  error_array(:)
    type(Contenedor), allocatable :: contenedores(:)
    ! Declaración de un arreglo de Tkn para almacenar los tokens
    type(Tkn), ALLOCATABLE ::  token_array(:)
    
    estado = 0
    puntero = 1
    columna = 1
    linea=1
    fila = 1
    

    contenido = ""
    
    
    !S = [':','{','}',';']
    !N=['0','1','2','3','4','5','6','7','8','9','%']
    
    
   

!---------------------lectura de contenido---------------------------
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

    call analizador(contenido)

contains

!----------------------analizador lexico ------------------------------------------------
    subroutine analizador(contenido)

    
    implicit none
    character(len=:), allocatable :: contenido
    integer :: i

   
    
    character(len=256) :: aux_tkn
    aux_tkn = ""
    
    !type(TokenInfo), dimension(500) :: tokens, errores
    print*, 'llego a analisis'
    print*, contenido
    len = len_trim(contenido)
    

        ! estados 
    do while (puntero <= len)
        char = contenido(puntero:puntero)

        select case (estado)
        case (0)
            
        
            ! Detectar comentarios de línea (//)
            if (char == '/') then
                puntero = puntero + 1
                char = contenido(puntero:puntero)  ! Avanza al siguiente carácter
        
                if (char == '/') then
                    print*,' Comentario de línea, ignorar hasta el fin de línea'

                    do while (ichar(char) /= 10)  ! 10 es el código ASCII para nueva línea
                        puntero = puntero + 1
                        char = contenido(puntero:puntero)
                    end do
                    fila = fila + 1
                    columna = 0
                    estado = 0
        
                    elseif (char == '*') then
                        ! Comentario de bloque, ignorar hasta encontrar */
                        print*,'abre comentario de bloque '
                        do
                            puntero = puntero + 1
                            
                            ! Verifica que puntero no exceda la longitud
                            if (puntero > len) then
                                call agregar_error('/*', '*/', fila, columna)
                                exit  ! Error: Comentario de bloque no cerrado
                            end if
                            
                            char = contenido(puntero:puntero)
                    
                            if (char == '*') then
                                puntero = puntero + 1
                                
                                ! Verifica nuevamente
                                if (puntero > len) then
                                    call agregar_error('/*', '*/', fila, columna)
                                    exit  ! Error: Comentario de bloque no cerrado
                                end if
                                
                                char = contenido(puntero:puntero)
                                
                                if (char == '/') then
                                    print*,'cierra comentario de bloque'
                                    puntero = puntero + 1
                                    exit  ! Fin del comentario de bloque
                                end if
                            elseif (ichar(char) == 10) then
                                ! Salto de línea dentro del comentario
                                fila = fila + 1
                                columna = 0
                            end if
                        end do
                        estado = 0
        
                else
                    ! No era comentario, proceso normal
                    estado = 1
                    columna = columna + 1
                end if
        
            ! Verifica que el carácter sea un símbolo
            elseif (char == ';' .or. char == '-' .or. char == '.' .or. &
                    char == '(' .or. char == ')' .or. char == ',' .or. &
                    char == '<' .or. char == '>' .or. char == '!') then
                estado = 1
                columna = columna + 1
        
            elseif (char >= 'A' .and. char <= 'Z' .or. (char >= 'a' .and. char <= 'z')) then
                estado = 2  ! Iniciamos la construcción de un identificador o palabra clave
        
            elseif (char >= '0' .and. char <= '9') then
                estado = 3  ! Estado para construir números
        
            elseif (char == '"') then
                    aux_tkn = ""  ! Iniciar el token literal sin comillas
                    columna = columna + 1
                    puntero = puntero + 1
                    estado = 4 
        
            elseif (ichar(char) == 10) then
                ! Salto de línea
                columna = 0
                fila = fila + 1
                puntero = puntero + 1
        
            elseif (ichar(char) == 9) then
                ! Tabulación
                columna = columna + 4
                puntero = puntero + 1
        
            elseif (ichar(char) == 32) then
                columna = columna + 1
                puntero = puntero + 1
        
            ! Si el carácter es inválido (no está en el conjunto permitido)
            else
                call agregar_error(char, 'Simbolo valido', fila, columna)
                columna = columna + 1
                puntero = puntero + 1
            end if
                
            case (1)
                
                if ( char == ';' ) then
                    call agregar_token(char, 'tk_pyc', fila, columna)
                    
                elseif ( char == '.' ) then
                    call agregar_token(char, 'tk_punto', fila, columna)

                elseif ( char == ',' ) then
                    call agregar_token(char, 'tk_coma', fila, columna)

                elseif ( char == '>') then
                    call agregar_token(char, 'tk_mayor', fila, columna)

                elseif ( char == '<') then
                    call agregar_token(char, 'tk_menor', fila, columna)

                elseif ( char == '(') then
                    call agregar_token(char, 'tk_par_izq', fila, columna)

                elseif ( char == ')') then
                    call agregar_token(char, 'tk_par_der', fila, columna)         
                
                elseif ( char == '-') then
                    call agregar_token(char, 'tk_guion', fila, columna)
                
                elseif ( char == '!') then
                    call agregar_token(char, 'tk_exp', fila, columna) 

                end if
                puntero = puntero + 1
                estado = 0

            case (2)
                
                if ( (char >= 'A' .and. char <= 'Z') .or. &
                    (char >= 'a' .and. char <= 'z') .or. &
                    (char >= '0' .and. char <= '9') ) then
                        aux_tkn = trim(aux_tkn) // char  ! Concatenar el carácter

                
                        columna = columna + 1
                        puntero = puntero + 1
                    
                else
                    if((trim(aux_tkn)== 'Controles')) then
                        call agregar_token(aux_tkn,'tk_control',fila,columna)
                        print*, 'se encontro  el control' 
                        print*,  aux_tkn

                    elseif((aux_tkn == 'propiedades')) then
                        call agregar_token(aux_tkn,'tk_propiedades',fila,columna)
                        print*,  aux_tkn
                    elseif((aux_tkn == 'Colocacion')) then
                        call agregar_token(aux_tkn,'tk_colocacion',fila,columna)
                        print*,  aux_tkn
                    elseif(aux_tkn == 'Contenedor') then
                        call agregar_token(aux_tkn, 'tk_contenedor', fila, columna)
                        print*,  aux_tkn
                        
                    elseif (aux_tkn == 'Etiqueta') then
                        call agregar_token(aux_tkn, 'tk_etiqueta', fila, columna)
                        print*,  aux_tkn
                    elseif (aux_tkn == 'Clave') then
                        call agregar_token(aux_tkn, 'tk_Clave', fila, columna)
                        print*,  aux_tkn
                    elseif (aux_tkn == 'Texto') then
                        call agregar_token(aux_tkn, 'tk_Texto', fila, columna)
                        print*,  aux_tkn
                    elseif (aux_tkn == 'Boton') then
                        call agregar_token(aux_tkn, 'tk_boton', fila, columna)
                        print*,  aux_tkn
                    elseif (aux_tkn == 'setAncho') then
                        call agregar_token(aux_tkn, 'tk_setAncho', fila, columna)
                        print*,  aux_tkn
                    elseif (aux_tkn == 'setAlto') then
                        call agregar_token(aux_tkn, 'tk_setAlto', fila, columna)
                    elseif (aux_tkn == 'setColorFondo') then
                        call agregar_token(aux_tkn, 'tk_setColorFondo', fila, columna)
                    elseif (aux_tkn == 'setColorLetra') then
                        call agregar_token(aux_tkn, 'tk_setColorLetra', fila, columna)
                    elseif (aux_tkn == 'setTexto') then
                        call agregar_token(aux_tkn, 'tk_setTexto', fila, columna)
                    elseif (aux_tkn == 'setPosicion') then
                        call agregar_token(aux_tkn, 'tk_setPosicion', fila, columna)
                    elseif (aux_tkn == 'this') then
                        call agregar_token(aux_tkn, 'tk_this', fila, columna)
                    elseif (aux_tkn == 'add') then
                        call agregar_token(aux_tkn, 'tk_add', fila, columna)
                    else
                        call agregar_token(aux_tkn, 'tk_id', fila, columna)
                    end if
                    aux_tkn = ""
                    estado = 0
                end if

            case (3)
                

                if (char >= '0' .and. char <= '9' ) then
                    aux_tkn = trim(aux_tkn) // char
                    columna = columna + 1
                    puntero = puntero + 1
                    
                else
                    call agregar_token(aux_tkn, 'tk_num', fila, columna)
                
                    aux_tkn = ""
                    estado = 0
                end if

            case (4)
                ! Captura el texto dentro de las comillas
                if (ichar(char) >= 0 .and. ichar(char) <= 255 .and. char .ne. '"') then
                    aux_tkn = trim(aux_tkn) // char  ! Concatenar el carácter dentro del literal
                    columna = columna + 1
                    puntero = puntero + 1 
                elseif (char == '"') then
                    ! Se cierra el literal, agregar el token
                    call agregar_token(aux_tkn, 'tk_literal', fila, columna)
                    aux_tkn = ""
                    puntero = puntero + 1
                    estado = 0  ! Volver al estado 0
                elseif (puntero == len) then
                    ! Error: No se cerraron las comillas
                    call agregar_error(aux_tkn, 'Error: comillas de cierre no encontradas', fila, columna)
                    estado = 0
                end if             

        end select
    end do
    call imprimir_errores()
    call parser()
    call imprimir_tokens()
    end subroutine analizador
!---------------------------tokens lista-----------------------------
    subroutine agregar_token(lexema, tipo, fila, columna)
        CHARACTER(LEN=*), INTENT(IN) :: lexema
        CHARACTER(LEN=*), INTENT(IN) :: tipo
        integer :: fila
        integer :: columna
        type(Tkn) :: nuevo_token
        integer :: n
        type(Tkn), ALLOCATABLE ::  temp_array(:)
        
        !Inicializo los datos del nuevo token
        nuevo_token%lexema = lexema
        nuevo_token%tipo = tipo
        nuevo_token%fila = fila
        nuevo_token%columna = columna

        ! Agrego el nuevo token a la lista de tokens
        if (.NOT. ALLOCATED(token_array)) then !Si esta vacia
            ALLOCATE(token_array(1)) ! Se le asigna memoria para un token de la lista
            token_array(1) =  nuevo_token !Se convierte en el token nuevo
        else
            n = size(token_array)
            ALLOCATE(temp_array(n+1))
            temp_array(:n) = token_array !Reservo memoria
            temp_array(n+1) = nuevo_token
            DEALLOCATE(token_array) !Libero memoria
            ALLOCATE(token_array(n+1)) !Reservo memoria de nuevo
            token_array = temp_array
        end if
    end subroutine agregar_token
    subroutine imprimir_tokens()
        integer :: i, contador_tokens
        character(len=20) :: str_fila, str_columna
        character(len=100) :: nombre_archivo
    
        ! Inicializar el contador de tokens
        contador_tokens = 0
    
        ! Especificar el nombre del archivo
        nombre_archivo = "tokens.txt"
    
        ! Abrir el archivo para escritura
        open(unit=10, file=nombre_archivo, status='replace', action='write')
    
        if (.NOT. ALLOCATED(token_array)) then
            write(10, *) "No hay tokens disponibles"
        else
            ! Loop para escribir cada token
            DO i = 1, size(token_array)
                contador_tokens = contador_tokens + 1
                write(str_fila, '(I0)') token_array(i)%fila
                write(str_columna, '(I0)') token_array(i)%columna
                write(10, *) 'Token ', i, ':'
                write(10, *) '  Lexema = ', trim(token_array(i)%lexema)
                write(10, *) '  Tipo = ', trim(token_array(i)%tipo)
                write(10, *) '  Fila = ', trim(str_fila)
                write(10, *) '  Columna = ', trim(str_columna)
                write(10, *) ""
            END DO
            
            ! Escribir el contador de tokens al final
            write(10, *) "Total de tokens procesados: ", contador_tokens
        end if
    
        ! Cerrar el archivo
        close(10)
    end subroutine imprimir_tokens
    
    
!---------------------------token error lista -----------------------------
    subroutine agregar_error(ultimo_token, token_esperado, fila, columna)
        CHARACTER(LEN=*), INTENT(IN) :: ultimo_token
        CHARACTER(LEN=*), INTENT(IN) :: token_esperado
        integer :: fila
        integer :: columna
        type(TknE) :: nuevo_error
        integer :: n
        type(TknE), ALLOCATABLE ::  temp_array(:)
        
        !Inicializo los datos del nuevo error
        nuevo_error%ultimo_token = ultimo_token
        nuevo_error%token_esperado = token_esperado
        nuevo_error%fila = fila
        nuevo_error%columna = columna

        ! Agrego el nuevo error a la lista de errores
        if (.NOT. ALLOCATED(error_array)) then !Si esta vacia
            ALLOCATE(error_array(1)) ! Se le asigna memoria para un error de la lista
            error_array(1) =  nuevo_error !Se convierte en el error nuevo
        else
            n = size(error_array)
            ALLOCATE(temp_array(n+1))
            temp_array(:n) = error_array !Reservo memoria
            temp_array(n+1) = nuevo_error
            DEALLOCATE(error_array) !Libero memoria
            ALLOCATE(error_array(n+1)) !Reservo memoria de nuevo
            error_array = temp_array
        end if

        
    end subroutine agregar_error

    ! Subrutina para imprimir los errores en consola
    subroutine imprimir_errores()
        integer :: i ! Contador del bucle
        character(len=20) :: str_fila, str_columna
        ! Número de unidad para el archivo
        character(len=100) :: nombre_archivo
    
        ! Especificar el nombre del archivo
        nombre_archivo = "errores.txt"
    
        ! Abrir el archivo para escritura
        open(unit=10, file=nombre_archivo, status='replace', action='write')
    
        if (.NOT. ALLOCATED(error_array)) then
            write(10, *) "No hay errores"
        else
            DO i = 1, size(error_array)
                write(str_fila, '(I0)') error_array(i)%fila
                write(str_columna, '(I0)') error_array(i)%columna
                write(10, *) 'Error Sintactico: '
                write(10, *) 'Ultimo Token: ', trim(error_array(i)%ultimo_token)
                write(10, *) 'Token Esperado: ', trim(error_array(i)%token_esperado)
                write(10, *) 'Fila: ', trim(str_fila)
                write(10, *) 'Columna: ', trim(str_columna)
                write(10, *) "" 
            END DO
        end if
    
        ! Cerrar el archivo
        close(10)

    end subroutine imprimir_errores
!-----------------------analizador sintactico------------------------------------
    subroutine agregar_contenedor(tipo, id)
        character(len=*), intent(in) :: tipo
        character(len=*), intent(in) :: id
        type(Contenedor) :: nuevo_contenedor
        integer :: n
        type(Contenedor), allocatable :: temp_array(:)
    
        ! Inicializo los datos del nuevo contenedor
        nuevo_contenedor%tipo = tipo
        nuevo_contenedor%id = id
    
        ! Agrego el nuevo contenedor al arreglo
        if (.NOT. allocated(contenedores)) then
            allocate(contenedores(1)) ! Se le asigna memoria para un contenedor
            contenedores(1) = nuevo_contenedor ! Se convierte en el contenedor nuevo
        else
            n = size(contenedores)
            allocate(temp_array(n + 1))
            temp_array(:n) = contenedores ! Copio los contenedores existentes
            temp_array(n + 1) = nuevo_contenedor
            deallocate(contenedores) ! Libero memoria
            allocate(contenedores(n + 1)) ! Reservo memoria de nuevo
            contenedores = temp_array
        end if
    end subroutine agregar_contenedor
    subroutine imprimir_contenedores()
        integer :: i
    
        if (.NOT. ALLOCATED(contenedores)) then
            print *, "No hay contenedores asignados."
            return
        end if
    
        print *, "Lista de Contenedores:"
        do i = 1, size(contenedores)
            print *, "Contenedor ", i, ": Tipo = ", contenedores(i)%tipo, ", ID = ", contenedores(i)%id
        end do
    end subroutine imprimir_contenedores
    
    subroutine crear_html(nombre_archivo)
        character(len=*), intent(in) :: nombre_archivo
        integer :: i
        integer :: unit
        integer :: ios  ! Variable para capturar el estado de la operación de archivo
    
        ! Abre el archivo HTML para escritura
        open(unit=unit, file=nombre_archivo, status='replace', action='write', iostat=ios)
    
        ! Verifica si hubo un error al abrir el archivo
        if (ios .ne. 0) then
            print *, "Error al abrir el archivo: ", nombre_archivo
            return
        end if
    
        ! Escribe la cabecera del HTML
        write(unit, *) '<!DOCTYPE html>'
        write(unit, *) '<html>'
        write(unit, *) '<head>'
        write(unit, *) '    <title>Contenedores</title>'
        write(unit, *) '</head>'
        write(unit, *) '<body>'
    
        ! Verifica si hay contenedores asignados
        if (.NOT. ALLOCATED(contenedores)) then
            write(unit, *) '    <p>No hay contenedores asignados.</p>'
        else
            ! Recorre el arreglo de contenedores y escribe cada uno
            do i = 1, size(contenedores)
                select case (trim(contenedores(i)%tipo))
                    case ('div')
                        write(unit, *) '    <div id="', trim(contenedores(i)%id), '"></div>'
                    case ('button')
                        write(unit, *) '    <button id="', trim(contenedores(i)%id), '"></button>'
                    case ('password')
                        write(unit, *) '    <input type="password" id="', trim(contenedores(i)%id), '" />'
                    case ('label')
                        write(unit, *) '    <label id="', trim(contenedores(i)%id), '"></label>'
                    case ('text')
                        write(unit, *) '    <input type="text" id="', trim(contenedores(i)%id), '" />'
                    ! Agregar más casos según sea necesario
                    case default
                        write(unit, *) '    <div>Tipo desconocido: ', trim(contenedores(i)%tipo), '</div>'
                end select
            end do
        end if
    
        ! Cierra el HTML
        write(unit, *) '</body>'
        write(unit, *) '</html>'
    
        ! Cierra el archivo
        close(unit)
    
        print *, "Archivo HTML creado: ", nombre_archivo
    end subroutine crear_html
    
    
    subroutine parser()
        integer :: i, j
        
        
        integer :: num_contenedores
        num_contenedores = 0
    
        ! Verifica si la memoria ha sido asignada para el arreglo
        if (.NOT. ALLOCATED(token_array)) then
            print *, "No hay tokens"
        else
            ! Verifica si el primer token es un comentario de inicio
            if (token_array(1)%tipo .ne. 'tk_menor' .or. &
                token_array(2)%tipo .ne. 'tk_exp' .or. &
                token_array(3)%tipo .ne. 'tk_guion' .or. &
                token_array(4)%tipo .ne. 'tk_guion') then
                call agregar_error(token_array(1)%lexema, 'tk_comentario', &
                                   token_array(1)%fila, token_array(1)%columna)
            end if
    
            print *, 'no hay problema con <!--'
    
            DO i = 5, size(token_array)
                ! Verifica si el token actual es una etiqueta de control
                if (token_array(i)%tipo == 'tk_control') then
                    ! Inicializa un nuevo índice para verificar los siguientes tokens
                    j = i + 1
    
                    ! Bucle para verificar los tokens siguientes
                    DO WHILE (j <= size(token_array) .and. &
                               (token_array(j)%tipo == 'tk_contenedor' .or. &
                                token_array(j)%tipo == 'tk_boton' .or. &
                                token_array(j)%tipo == 'tk_Clave' .or. &
                                token_array(j)%tipo == 'tk_etiqueta' .or. &
                                token_array(j)%tipo == 'tk_Texto'))
    
                        print *, token_array(j)%tipo
    
                        ! Verifica que el siguiente token después del tipo de control sea un identificador
                        if (j + 1 <= size(token_array) .and. &
                            token_array(j + 1)%tipo == 'tk_id') then
    
                            ! Verifica que el siguiente token sea un punto y coma
                            if (j + 2 <= size(token_array) .and. &
                                token_array(j + 2)%tipo == 'tk_pyc') then
                                
                                ! Aquí se agrega la lógica para manejar el control válido
                                
    
                                if (token_array(j)%tipo == 'tk_boton') then
                                    call agregar_contenedor('button', token_array(j + 1)%lexema)
                                else if (token_array(j)%tipo == 'tk_contenedor') then
                                    call agregar_contenedor('div', token_array(j + 1)%lexema)
                                else if (token_array(j)%tipo == 'tk_Clave') then
                                    call agregar_contenedor('password', token_array(j + 1)%lexema)
                                else if (token_array(j)%tipo == 'tk_etiqueta') then
                                    call agregar_contenedor('label', token_array(j + 1)%lexema)
                                else if (token_array(j)%tipo == 'tk_Texto') then
                                    call agregar_contenedor('text', token_array(j + 1)%lexema)
                                end if
    
                                ! Actualiza j para saltar al siguiente grupo de tokens
                                j = j + 3
                            else
                                call agregar_error(token_array(j + 2)%lexema, 'tk_pyc', &
                                                   token_array(j + 2)%fila, token_array(j + 2)%columna)
                                exit  ! Sale del bucle si hay un error
                            end if
                        else
                            call agregar_error(token_array(j + 1)%lexema, 'tk_id', &
                                               token_array(j + 1)%fila, token_array(j + 1)%columna)
                            exit  ! Sale del bucle si hay un error
                        end if
                    END DO
    
                    ! Actualiza i para continuar después de los tokens procesados
                    ! Ajusta i para que no vuelva a procesar los mismos tokens
                end if
            END DO
    
            i = j - 1
    
            if (token_array(i)%tipo .ne. 'tk_control' .or. &
                token_array(i + 1)%tipo .ne. 'tk_guion' .or. &
                token_array(i + 2)%tipo .ne. 'tk_guion' .or. &
                token_array(i + 3)%tipo .ne. 'tk_mayor') then
                call agregar_error(token_array(i)%lexema, 'tk_comentario', &
                                   token_array(i)%fila, token_array(i)%columna)
            else 
                i = j + 3
                call parser_propiedades(i)
            end if
        end if
        call imprimir_errores()
        call imprimir_contenedores()
        call crear_html('pagina.html')
    end subroutine parser
    
    
    subroutine parser_propiedades(i)
        integer :: i, j
    
        ! Verifica el inicio de las propiedades
        if (token_array(i)%tipo .ne. 'tk_menor' .or. token_array(i+1)%tipo .ne. 'tk_exp' .or. &
            token_array(i+2)%tipo .ne. 'tk_guion' .or. token_array(i+3)%tipo .ne. 'tk_guion') then
            call agregar_error(token_array(i)%lexema, 'tk_comentario', &
                               token_array(i)%fila, token_array(i)%columna)
        
                            end if
        print*, 'no hay problema con <!--'
    
        ! Loop sobre los tokens, comenzando desde el índice i + 4
        DO i = i + 4, size(token_array)
            
            if (token_array(i)%tipo == 'tk_propiedades') then
                j = i + 1
                print *, 'hay propiedades'
                
                ! Loop para procesar múltiples propiedades
                DO WHILE (j < size(token_array) .and. token_array(j)%tipo .ne. 'tk_propiedades') 
                    if (token_array(j)%tipo == 'tk_id' .and. token_array(j+1)%tipo == 'tk_punto') then
                        
        
                        ! Procesar setAncho
                        if (token_array(j+2)%tipo == 'tk_setAncho') then
                            if (token_array(j+3)%tipo .ne. 'tk_par_izq') then
                                call agregar_error(token_array(j+3)%lexema, 'tk_par_izq', &
                                                   token_array(j+3)%fila, token_array(j+3)%columna)
        
                            elseif (token_array(j+4)%tipo .ne. 'tk_num') then
                                call agregar_error(token_array(j+4)%lexema, 'tk_num', &
                                                   token_array(j+4)%fila, token_array(j+4)%columna)
        
                            elseif (token_array(j+5)%tipo .ne. 'tk_par_der') then
                                call agregar_error(token_array(j+5)%lexema, 'tk_par_der', &
                                                   token_array(j+5)%fila, token_array(j+5)%columna)
        
                            elseif (token_array(j+6)%tipo .ne. 'tk_pyc') then
                                call agregar_error(token_array(j+6)%lexema, 'tk_pyc', &
                                                   token_array(j+6)%fila, token_array(j+6)%columna)
        
                            else
                                print*, 'sin errores 1'
                                j = j + 6  ! Actualiza j para avanzar
                            end if
                        end if
        
                        ! Procesar setAlto
                        if (token_array(j+2)%tipo == 'tk_setAlto') then
                            if (token_array(j+3)%tipo .ne. 'tk_par_izq') then
                                call agregar_error(token_array(j+3)%lexema, 'tk_par_izq', &
                                                   token_array(j+3)%fila, token_array(j+3)%columna)
        
                            elseif (token_array(j+4)%tipo .ne. 'tk_num') then
                                call agregar_error(token_array(j+4)%lexema, 'tk_num', &
                                                   token_array(j+4)%fila, token_array(j+4)%columna)
        
                            elseif (token_array(j+5)%tipo .ne. 'tk_par_der') then
                                call agregar_error(token_array(j+5)%lexema, 'tk_par_der', &
                                                   token_array(j+5)%fila, token_array(j+5)%columna)
        
                            elseif (token_array(j+6)%tipo .ne. 'tk_pyc') then
                                call agregar_error(token_array(j+6)%lexema, 'tk_pyc', &
                                                   token_array(j+6)%fila, token_array(j+6)%columna)
        
                            else
                                print*, 'sin errores 2'
                                j = j + 6  ! Actualiza j para avanzar
                            end if
                        end if
        
                        ! Procesar setColorLetra
                        if (token_array(j+2)%tipo == 'tk_setColorLetra') then
                            if (token_array(j+3)%tipo .ne. 'tk_par_izq') then
                                call agregar_error(token_array(j+3)%lexema, 'tk_par_izq', &
                                                   token_array(j+3)%fila, token_array(j+3)%columna)
        
                            elseif (token_array(j+4)%tipo .ne. 'tk_num') then
                                call agregar_error(token_array(j+4)%lexema, 'tk_num', &
                                                   token_array(j+4)%fila, token_array(j+4)%columna)
        
                            elseif (token_array(j+5)%tipo .ne. 'tk_coma') then
                                call agregar_error(token_array(j+5)%lexema, 'tk_coma', &
                                                   token_array(j+5)%fila, token_array(j+5)%columna)
        
                            elseif (token_array(j+6)%tipo .ne. 'tk_num') then
                                call agregar_error(token_array(j+6)%lexema, 'tk_num', &
                                                   token_array(j+6)%fila, token_array(j+6)%columna)
        
                            elseif (token_array(j+7)%tipo .ne. 'tk_coma') then
                                call agregar_error(token_array(j+7)%lexema, 'tk_coma', &
                                                   token_array(j+7)%fila, token_array(j+7)%columna)
        
                            elseif (token_array(j+8)%tipo .ne. 'tk_num') then
                                call agregar_error(token_array(j+8)%lexema, 'tk_num', &
                                                   token_array(j+8)%fila, token_array(j+8)%columna)
        
                            elseif (token_array(j+9)%tipo .ne. 'tk_par_der') then
                                call agregar_error(token_array(j+9)%lexema, 'tk_par_der', &
                                                   token_array(j+9)%fila, token_array(j+9)%columna)
        
                            elseif (token_array(j+10)%tipo .ne. 'tk_pyc') then
                                call agregar_error(token_array(j+10)%lexema, 'tk_pyc', &
                                                   token_array(j+10)%fila, token_array(j+10)%columna)
        
                            else
                                print*, 'sin errores 3'
                                j = j + 10  ! Actualiza j para avanzar
                            end if
                        end if
        
                        ! Procesar setTexto
                        if (token_array(j+2)%tipo == 'tk_setTexto') then
                            if (token_array(j+3)%tipo .ne. 'tk_par_izq') then
                                call agregar_error(token_array(j+3)%lexema, 'tk_par_izq', &
                                                   token_array(j+3)%fila, token_array(j+3)%columna)
        
                            elseif (token_array(j+4)%tipo .ne. 'tk_literal') then
                                call agregar_error(token_array(j+4)%lexema, 'tk_literal', &
                                                   token_array(j+4)%fila, token_array(j+4)%columna)
        
                            elseif (token_array(j+5)%tipo .ne. 'tk_par_der') then
                                call agregar_error(token_array(j+5)%lexema, 'tk_par_der', &
                                                   token_array(j+5)%fila, token_array(j+5)%columna)
        
                            elseif (token_array(j+6)%tipo .ne. 'tk_pyc') then
                                call agregar_error(token_array(j+6)%lexema, 'tk_pyc', &
                                                   token_array(j+6)%fila, token_array(j+6)%columna)
        
                            else
                                print*, 'sin errores 4'
                                j = j + 6  ! Actualiza j para avanzar
                            end if
                        end if
                        
                        ! Aquí puedes agregar condiciones similares para setPosicion, etc.
                        
                    end if
                    j = j + 1  ! Avanza al siguiente token para seguir buscando
                END DO
                if (j < size(token_array) .and. token_array(j)%tipo == 'tk_propiedades') then
                    print*, 'Se encontró otro tk_propiedades, saliendo del bucle.'
                    exit  ! Salir del bucle
                end if
            end if
            
        END DO
        print*,' luego de parser', j
        i=j
        print*,'valor i ',i
        if (token_array(i)%tipo .ne. 'tk_propiedades' .or. &
            token_array(i+1)%tipo .ne. 'tk_guion' .or. &
            token_array(i+2)%tipo .ne. 'tk_guion' .or. &
            token_array(i+3)%tipo .ne. 'tk_mayor') then
            call agregar_error(token_array(i)%lexema, 'tk_comentario -->', &
                               token_array(i)%fila, token_array(i)%columna)
            
            else 
                i=j+3
                call parser_colocacion(i+1)
            end if
        call imprimir_errores()
    end subroutine parser_propiedades
    
    subroutine parser_colocacion(i)
        integer :: i, j
    
        ! Verifica el inicio de la colocación
        if (token_array(i)%tipo .ne. 'tk_menor' .or. token_array(i+1)%tipo .ne. 'tk_exp' .or. &
            token_array(i+2)%tipo .ne. 'tk_guion' .or. token_array(i+3)%tipo .ne. 'tk_guion') then
            call agregar_error(token_array(i)%lexema, 'tk_comentario', &
                               token_array(i)%fila, token_array(i)%columna)
            
        end if
        print*, 'no hay problema con <!--Colocacion'
    
        ! Loop sobre los tokens, comenzando desde el índice i + 4
        DO i = i + 4, size(token_array)
    
            if (token_array(i)%tipo == 'tk_colocacion') then
                j = i + 1
                print *, 'hay colocaciones'
    
                ! Loop para procesar múltiples colocaciones
                DO WHILE (j < size(token_array) .and. token_array(j)%tipo .ne. 'tk_colocacion') 
                    if (token_array(j)%tipo == 'tk_id' .and. token_array(j+1)%tipo == 'tk_punto') then
                        
                        ! Procesar setPosicion
                         if(token_array(j+2)%tipo == 'tk_setPosicion') then
                            if (token_array(j+3)%tipo .ne. 'tk_par_izq') then
                            call agregar_error(token_array(j+3)%lexema, 'tk_par_izq', &
                                                   token_array(j+3)%fila, token_array(j+3)%columna)
    
                            elseif (token_array(j+4)%tipo .ne. 'tk_num') then
                                call agregar_error(token_array(j+4)%lexema, 'tk_num', &
                                                   token_array(j+4)%fila, token_array(j+4)%columna)
    
                            elseif (token_array(j+5)%tipo .ne. 'tk_coma') then
                                call agregar_error(token_array(j+5)%lexema, 'tk_coma', &
                                                   token_array(j+5)%fila, token_array(j+5)%columna)
    
                            elseif (token_array(j+6)%tipo .ne. 'tk_num') then
                                call agregar_error(token_array(j+6)%lexema, 'tk_num', &
                                                   token_array(j+6)%fila, token_array(j+6)%columna)
    
                            elseif (token_array(j+7)%tipo .ne. 'tk_par_der') then
                                call agregar_error(token_array(j+7)%lexema, 'tk_par_der', &
                                                   token_array(j+7)%fila, token_array(j+7)%columna)
    
                            elseif (token_array(j+8)%tipo .ne. 'tk_pyc') then
                                call agregar_error(token_array(j+8)%lexema, 'tk_pyc', &
                                                   token_array(j+8)%fila, token_array(j+8)%columna)
    
                            else
                                print*, 'sin errores en setPosicion'
                                j = j + 8  ! Actualiza j para avanzar
                            end if
                            
                        end if
                        if(token_array(j+2)%tipo == 'tk_add') then
                            if (token_array(j+3)%tipo .ne. 'tk_par_izq') then
                                call agregar_error(token_array(j+3)%lexema, 'tk_par_izq', &
                                                   token_array(j+3)%fila, token_array(j+3)%columna)
                            elseif (token_array(j+4)%tipo .ne. 'tk_id') then
                            call agregar_error(token_array(j+4)%lexema, 'tk_id', &
                                                token_array(j+4)%fila, token_array(j+4)%columna)
                            elseif (token_array(j+5)%tipo .ne. 'tk_par_der') then
                                call agregar_error(token_array(j+5)%lexema, 'tk_par_der', &
                                                    token_array(j+5)%fila, token_array(j+5)%columna)
    
                            elseif (token_array(j+6)%tipo .ne. 'tk_pyc') then
                                call agregar_error(token_array(j+6)%lexema, 'tk_pyc', &
                                                    token_array(j+6)%fila, token_array(j+6)%columna)
    
                            else
                                print*, 'sin errores en setPosicion'
                                j = j + 6  ! Actualiza j para avanzar
                            end if
                        end if
                    elseif  (token_array(j)%tipo == 'tk_this' .and. token_array(j+1)%tipo == 'tk_punto') then
                        if(token_array(j+2)%tipo == 'tk_add') then
                            if (token_array(j+3)%tipo .ne. 'tk_par_izq') then
                                call agregar_error(token_array(j+3)%lexema, 'tk_par_izq', &
                                                   token_array(j+3)%fila, token_array(j+3)%columna)
                            elseif (token_array(j+4)%tipo .ne. 'tk_id') then
                            call agregar_error(token_array(j+4)%lexema, 'tk_id', &
                                                token_array(j+4)%fila, token_array(j+4)%columna)
                            elseif (token_array(j+5)%tipo .ne. 'tk_par_der') then
                                call agregar_error(token_array(j+5)%lexema, 'tk_par_der', &
                                                    token_array(j+5)%fila, token_array(j+5)%columna)
    
                            elseif (token_array(j+6)%tipo .ne. 'tk_pyc') then
                                call agregar_error(token_array(j+6)%lexema, 'tk_pyc', &
                                                    token_array(j+6)%fila, token_array(j+6)%columna)
    
                            else
                                print*, 'sin errores en setPosicion'
                                j = j + 6  ! Actualiza j para avanzar
                            end if
                        end if
                        ! Aquí puedes agregar condiciones similares para otros métodos de colocación
                    end if
                    j = j + 1  ! Avanza al siguiente token para seguir buscando
                END DO
                
                if (j < size(token_array) .and. token_array(j)%tipo == 'tk_colocacion') then
                    print*, 'Se encontró otro tk_colocacion, saliendo del bucle.'
                    exit  ! Salir del bucle
                end if
            end if
        END DO
        
        print*,' luego de parser_colocacion', j
        i = j
        print*,'valor i ', i
    
        if (token_array(i)%tipo .ne. 'tk_colocacion' .or. &
            token_array(i+1)%tipo .ne. 'tk_guion' .or. &
            token_array(i+2)%tipo .ne. 'tk_guion' .or. &
            token_array(i+3)%tipo .ne. 'tk_mayor') then
            call agregar_error(token_array(i)%lexema, 'tk_colocacion -->', &
                               token_array(i)%fila, token_array(i)%columna)
    
        else 
            i = j + 3
            ! Llama a la siguiente subrutina, si es necesario
            ! call parser_siguiente(i+1)
        end if
        call imprimir_errores()
    end subroutine parser_colocacion
    

end program main