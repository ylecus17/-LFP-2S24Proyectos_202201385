import tkinter as tk
from tkinter import filedialog, Scrollbar, messagebox
from tkinter import ttk
import subprocess
import os

# Variables globales
archivo_actual = None  # Guardará la ruta del archivo abierto

# Función para ejecutar el programa Fortran y enviar los datos
def enviar_datos():
    data = texto.get("1.0", tk.END)
    
    # Compilar el programa Fortran
    subprocess.run(
        ["gfortran", "-o", "main.exe", "main.f90"],  # Compilación de Fortran
        check=True  # Detener si hay un error en la compilación
    )
    
    # Ejecutar el programa Fortran y enviarle los datos directamente (sin archivo)
    resultado = subprocess.run(
        ["./main.exe"],  # Ejecutable de Fortran
        input=data,  # Enviar los datos a través de stdin
        text=True,  # Indicar que el input es texto (en lugar de bytes)
    )
    
    for row in tree.get_children():
        tree.delete(row)
    limpiar_tabla()
    # Cargar errores en la tabla
    cargar_errores()

# Función para abrir una nueva ventana con la tabla de tokens

def mostrar_tokens():
    ventana_tokens = tk.Toplevel()
    ventana_tokens.title("Tokens")
    ventana_tokens.geometry("600x400")
    
    tree_tokens = ttk.Treeview(ventana_tokens, columns=("Token", "Lexema", "Tipo", "Fila", "Columna"), show="headings")
    tree_tokens.pack(fill="both", expand=True)
    
    # Configurar los encabezados de la tabla
    tree_tokens.heading("Token", text="Token")
    tree_tokens.heading("Lexema", text="Lexema")
    tree_tokens.heading("Tipo", text="Tipo")
    tree_tokens.heading("Fila", text="Fila")
    tree_tokens.heading("Columna", text="Columna")
    
    # Ajustar el ancho de las columnas
    tree_tokens.column("Token", width=50)
    tree_tokens.column("Lexema", width=150)
    tree_tokens.column("Tipo", width=150)
    tree_tokens.column("Fila", width=50)
    tree_tokens.column("Columna", width=50)

    tokens = []  # Lista para almacenar los tokens

    try:
        with open("tokens.txt", "r") as file:
            token_num = 0  # Contador de tokens
            while True:
                line = file.readline()
                if not line:  # Si no hay más líneas, salir
                    break
                
                if "Token" in line:
                    token_num += 1  # Incrementar el contador de tokens

                    # Leer las líneas correspondientes al token
                    lexema_line = file.readline().strip()
                    tipo_line = file.readline().strip()
                    fila_line = file.readline().strip()
                    columna_line = file.readline().strip()

                    # Procesar las líneas para extraer los valores
                    lexema = lexema_line.split('= ')[1] if '= ' in lexema_line else ''
                    tipo = tipo_line.split('= ')[1] if '= ' in tipo_line else ''
                    fila = fila_line.split('= ')[1] if '= ' in fila_line else ''
                    columna = columna_line.split('= ')[1] if '= ' in columna_line else ''

                    # Agregar a la lista de tokens
                    tokens.append({
                        "token": token_num,
                        "lexema": lexema,
                        "tipo": tipo,
                        "fila": fila,
                        "columna": columna
                    })

        # Insertar tokens en la tabla
        for token in tokens:
            tree_tokens.insert("", tk.END, values=(token["token"], token["lexema"], token["tipo"], token["fila"], token["columna"]))

        if not tokens:
            messagebox.showinfo("Información", "No se encontraron tokens en el archivo.")
    
    except FileNotFoundError:
        messagebox.showerror("Error", "El archivo de tokens no se encontró.")
    except Exception as e:
        messagebox.showerror("Error", f"Ocurrió un error al cargar los tokens: {str(e)}")


# Función para limpiar la tabla de errores
def limpiar_tabla():
    for row in tree.get_children():
        tree.delete(row)

# Función para cargar errores desde el archivo
def cargar_errores():
    # Limpiar la tabla antes de cargar nuevos errores
    limpiar_tabla()

    errores = []  # Lista para almacenar errores

    # Leer el archivo de errores
    try:
        with open("errores.txt", "r") as file:
            for line in file:
                if "Error Sintactico:" in line:
                    # Crear un diccionario para cada error
                    error_info = {
                        "error_sintactico": line.strip(),
                        "ultimo_token": next(file).strip(),
                        "token_esperado": next(file).strip(),
                        "fila": next(file).strip(),
                        "columna": next(file).strip(),
                    }

                    # Agregar el diccionario a la lista de errores
                    errores.append(error_info)

                    # Leer la línea en blanco que separa los errores
                    next(file)  # Asume que hay una línea en blanco después de cada error

        # Insertar todos los errores en la tabla de una vez
        for error in errores:
            tree.insert("", tk.END, values=(error["error_sintactico"], error["ultimo_token"],
                                             error["token_esperado"], error["fila"], error["columna"]))

    except FileNotFoundError:
        messagebox.showerror("Error", "El archivo de errores no se encontró.")
    except Exception as e:
        messagebox.showerror("Error", f"Ocurrió un error al cargar los errores: {str(e)}")

# Función para abrir archivos de texto
def abrir_archivo():
    global archivo_actual
    archivo = filedialog.askopenfilename(
        title="Abrir archivo",
        filetypes=[("Archivos de texto", "*.lfp"), ("Todos los archivos", "*.*")]
    )
    
    if archivo:
        archivo_actual = archivo  # Guardar la ruta del archivo actual
        with open(archivo, "r") as file:
            contenido = file.read()
            texto.delete("1.0", tk.END)
            texto.insert(tk.END, contenido)

# Función para guardar archivos
def guardar_archivo():
    global archivo_actual
    if archivo_actual:  # Si ya hay un archivo abierto, sobreescribirlo
        with open(archivo_actual, "w") as file:
            contenido = texto.get("1.0", tk.END)
            file.write(contenido)
    else:
        guardar_como()  # Si no hay archivo, pedir guardar como uno nuevo

# Función para "Guardar como"
def guardar_como():
    global archivo_actual
    archivo = filedialog.asksaveasfilename(
        title="Guardar archivo como",
        defaultextension=".org",
        filetypes=[("Archivos de texto", "*.org"), ("Todos los archivos", "*.*")]
    )
    
    if archivo:
        archivo_actual = archivo  # Guardar la nueva ruta del archivo
        with open(archivo, "w") as file:
            contenido = texto.get("1.0", tk.END)
            file.write(contenido)

# Función para mostrar la información del estudiante
def mostrar_acerca_de():
    messagebox.showinfo("Acerca de", "Nombre: Natalia Garrido\nCarnet: 202201385")

# Función para salir de la aplicación
def salir():
    window.quit()

# Configuración de la ventana de Tkinter
window = tk.Tk()
window.title("Analizador Léxico")
window.geometry("1200x800")
window.config(bg="#737AF0")

# Crear la barra de menú
menu = tk.Menu(window)
submenu = tk.Menu(menu, tearoff=0)

# Opción para abrir, guardar y guardar como archivos
submenu.add_command(label="Open", command=abrir_archivo)
submenu.add_command(label="Save", command=guardar_archivo)
submenu.add_command(label="Save As", command=guardar_como)
submenu.add_command(label="Exit", command=salir)  # Opción para salir de la aplicación

menu.add_cascade(label="File", menu=submenu)

# Opción del menú Acerca de
menu.add_command(label="Acerca de", command=mostrar_acerca_de)

window.config(menu=menu)

# Crear las áreas de texto con scrollbars
scroll_texto = Scrollbar(window)
texto = tk.Text(window, height=20, width=80)  # Aumentar la anchura del área de texto
texto.pack(fill="x")  # Ajustar el tamaño del área de texto
scroll_texto.pack(side="right", fill="y")
scroll_texto.config(command=texto.yview)
texto.config(yscrollcommand=scroll_texto.set)

# Botón para ejecutar el análisis
tk.Button(window, text="Analizar", command=enviar_datos).pack()

# Botón para abrir la ventana de tokens
tk.Button(window, text="Mostrar Tokens", command=mostrar_tokens).pack()

# Crear una tabla para mostrar errores
tree = ttk.Treeview(window, columns=("Tipo de Error", "Último Token", "Token Esperado", "Fila", "Columna"), show="headings")
tree.pack(fill="both", expand=True)

# Configurar los encabezados de las columnas
tree.heading("Tipo de Error", text="Tipo de Error")
tree.heading("Último Token", text="Último Token")
tree.heading("Token Esperado", text="Token Esperado")
tree.heading("Fila", text="Fila")
tree.heading("Columna", text="Columna")

# Ajustar el ancho de las columnas
tree.column("Tipo de Error", width=200)
tree.column("Último Token", width=150)
tree.column("Token Esperado", width=150)
tree.column("Fila", width=50)
tree.column("Columna", width=50)

# Ejecutar el bucle principal de la ventana
window.mainloop()
