program analizador_lexico
    implicit none
    integer :: i, len, linea, columna, estado, puntero, numErrores, numTokens, file_unit, ios
    type :: TokenInfo
    character(len=200) :: lexema  ! Lexema (token)
    character(len=100) :: descripcion  ! Descripción o tipo del token
    integer :: columna      ! Columna donde ocurrió el token
    integer :: linea        ! Línea donde ocurrió el token
end type TokenInfo

type(TokenInfo), dimension(100) :: tokens, errores
    character(len=1) :: char 
    character(len=100) :: tkn
    character(len=1), dimension(26) :: A 
    character(len=1), dimension(26) :: M
    character(len=1), dimension(4) :: S 
    character(len=1), dimension(11) :: N
    character(len=10), dimension(7) :: palabrasReservadas
    

    character(len=10000) :: buffer, contenido
  
    contenido = ''  ! Inicializa contenido vacío
    palabrasReservadas = ['grafica   ', 'nombre    ','continente','pais      ','bandera   ','poblacion ','saturacion']  ! Palabras reservadas

    ! Lee el contenido desde la entrada estándar
    do
        read(*, '(A)', IOSTAT=ios) buffer
        if (ios /= 0) exit 
        contenido = trim(contenido) // trim(buffer) // new_line('a') ! concatenamos el contenido
    end do

    A = ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z']
    M = ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']
    S = [':','{','}',';']
    N=['0','1','2','3','4','5','6','7','8','9','%']
    estado = 0
    puntero = 1
    columna = 1
    linea = 1
    numErrores = 0
    numTokens = 0
    tkn = ""
    

    len = len_trim(contenido)

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
    else
        call generar_html_tokens(numTokens, tokens)
    end if
print*, 'analizado'
contains

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
        character(len=100) :: str_descripcion, str_columna, str_linea,char_error

        integer :: file_unit, ios, i

        ! Si hay errores, se crea el archivo HTML
       if (numErrores > 0) then
    ! Abrir el archivo para escribir
            open(unit=file_unit, file="errores.html", status="replace", action="write", iostat=ios)
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
                write(file_unit, '(A)') '</style></head><body><h2>Tabla de Errores</h2>' // new_line('a')
                write(file_unit, '(A)') '<table><tr><th>Carácter</th><th>Descripcion' 
                write(file_unit, '(A)') '</th><th>Columna</th><th>Línea</th></tr>' // new_line('a')

                ! Bucle para formatear cada código ASCII y cada columna

                ! Bucle para agregar filas a la tabla
                do i = 1, numErrores
                    write(str_descripcion, '(A)') trim(errores(i)%descripcion)
                    write(str_columna, '(I0)') errores(i)%columna
                    write(str_linea, '(I0)')  errores(i)%linea
                    write(char_error, '(A)') trim(errores(i)%lexema)
         
                    ! Escribir cada fila directamente al archivo

                    write(file_unit, '(A)') '<tr><td>' // char_error // '</td><td>' // trim(str_descripcion) // & 
                    '</td><td>' // trim(str_columna) // '</td><td>'&
                     // trim(str_linea) // '</td></tr>' // new_line('a')
                end do

                ! Cerrar la tabla y el HTML
                write(file_unit, '(A)') '</table></body></html>'
                close(file_unit)
            end if
        else
            print *, "No hay errores en el código."
        end if
    end subroutine generar_html_errores


end program analizador_lexico