import tkinter as tk
from tkinter import filedialog, Menu, Scrollbar
import subprocess

def enviar_datos():
    # Limpiar el área de texto derecha antes de analizar
    grafica.delete("1.0", tk.END)
    
    # Obtener el contenido del área de texto izquierda
    data = texto.get("1.0", tk.END)
    
    # Compilar y ejecutar el código Fortran
    comando = subprocess.run(
        ["gfortran", "-o", "analizadoraux.exe", "analizadoraux.f90"],  # compilación de Fortran
        check=True  # detener si hay un error en la compilación
    )
    resultado = subprocess.run(
        ["./analizadoraux.exe"],  # Ejecutable de Fortran
        input=data,  # la data que se manda a Fortran
        stdout=subprocess.PIPE,  # la data que viene de Fortran   
        text=True  # la salida se maneja como texto
    )
    
    # Insertar la nueva salida en el área de texto derecha
    grafica.insert(tk.END, resultado.stdout)

def abrir_archivo():
    # Abrir un cuadro de diálogo para seleccionar un archivo
    archivo = filedialog.askopenfilename(
        title="Abrir archivo",
        filetypes=[("Archivos de texto", "*.org"), ("Todos los archivos", "*.*")]
    )
    
    if archivo:
        with open(archivo, "r") as file:
            contenido = file.read()
            # Borrar el contenido anterior y cargar el archivo en el área de texto izquierda
            texto.delete("1.0", tk.END)
            texto.insert(tk.END, contenido)

window = tk.Tk()

# Establecer el título y el tamaño de la ventana
window.title("Analizador Léxico")
window.geometry("600x400")

# Cambiar el fondo de la ventana
window.config(bg="#737AF0")

# Crear la barra de menú
menu = tk.Menu(window)

# Crear las áreas de texto con scrollbars
scroll_texto = Scrollbar(window)
scroll_grafica = Scrollbar(window)

# Área de texto para la entrada de datos
texto = tk.Text(window, height=20, width=30, yscrollcommand=scroll_texto.set)
scroll_texto.config(command=texto.yview)
texto.place(x=10, y=30)
scroll_texto.place(x=280, y=30, height=320)

# Área de texto para la salida (análisis gráfico)
grafica = tk.Text(window, height=10, width=30, bg="white", yscrollcommand=scroll_grafica.set)
scroll_grafica.config(command=grafica.yview)
grafica.place(x=350, y=30)
scroll_grafica.place(x=620, y=30, height=160)

# Añadir ítems al menú
submenu = tk.Menu(menu, tearoff=0)
submenu.add_command(label="Open", command=abrir_archivo)
submenu.add_command(label="Save", command=lambda: print("Save clicked!"))
menu.add_cascade(label="File", menu=submenu)
menu.add_command(label="Edit", command=lambda: print("Edit menu clicked!"))
window.config(menu=menu)

# Botón para ejecutar el análisis
tk.Button(window, text="Analizar", command=enviar_datos).pack()

# Ejecutar el bucle principal de la ventana
window.mainloop()