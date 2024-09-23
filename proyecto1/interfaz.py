import tkinter as tk
from tkinter import filedialog, Scrollbar, messagebox
import subprocess
import os

# Variables globales
archivo_actual = None  # Guardará la ruta del archivo abierto

# Función para limpiar los archivos gráficos previos y la gráfica de la ventana
def limpiar_archivos_grafica():
    dot_path = os.path.abspath("grafica.dot")
    png_path = os.path.abspath("grafica.png")
    
    try:
        if os.path.exists(dot_path):
            os.remove(dot_path)
            print(f"El archivo {dot_path} ha sido eliminado.")
        else:
            print(f"El archivo {dot_path} no existe.")
        
        if os.path.exists(png_path):
            os.remove(png_path)
            print(f"El archivo {png_path} ha sido eliminado.")
            limpiar_grafica_ventana()  # Limpiar la gráfica en la ventana
        else:
            print(f"El archivo {png_path} no existe.")
    
    except PermissionError:
        print("No se tienen permisos para eliminar los archivos.")
    except OSError as e:
        print(f"Error al intentar eliminar archivos: {e}")

# Función para limpiar la gráfica de la ventana
def limpiar_grafica_ventana():
    label_imagen.config(image='')  # Limpiar la imagen del Label
    label_imagen.image = None  # Eliminar la referencia a la imagen
    print("La gráfica ha sido eliminada de la ventana.")

# Función para ejecutar el programa Fortran y enviar los datos
def enviar_datos():
    # Obtener el contenido del área de texto izquierda
    data = texto.get("1.0", tk.END)
    limpiar_archivos_grafica()
    
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
        capture_output=True,  # Capturar la salida del programa
        check=True  # Detener si hay un error en la ejecución
    )
    
    # Verificar si hay errores en la salida de Fortran
    if "Error" in resultado.stdout:
        print("Se encontraron errores en el análisis.")
        messagebox.showwarning("Errores", "Se encontraron errores en el análisis.")
        limpiar_grafica_ventana()  # Limpiar la gráfica si hay errores
    else:
        # Si no hay errores, generar la gráfica
        print("Análisis exitoso, generando gráfica.")
        generar_grafica()

# Función para abrir archivos de texto
def abrir_archivo():
    global archivo_actual
    archivo = filedialog.askopenfilename(
        title="Abrir archivo",
        filetypes=[("Archivos de texto", "*.org"), ("Todos los archivos", "*.*")]
    )
    
    if archivo:
        archivo_actual = archivo  # Guardar la ruta del archivo actual
        with open(archivo, "r") as file:
            contenido = file.read()
            # Borrar el contenido anterior y cargar el archivo en el área de texto izquierda
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

# Función para generar la gráfica usando Graphviz
def generar_grafica():
    # Comprobar si el archivo DOT existe en la ruta actual
    dot_path = os.path.abspath("grafica.dot")
    print(f"Buscando archivo DOT en: {dot_path}")
    
    if os.path.exists(dot_path):
        # Usar Graphviz para generar la imagen del archivo DOT
        os.system('dot -Gsize=10,7.5 -Tpng grafica.dot -o grafica.png')
        mostrar_grafica('grafica.png')
    else:
        print("El archivo grafica.dot no existe. No se puede generar la gráfica.")

# Función para mostrar la gráfica generada
def mostrar_grafica(imagen_path):
    # Cargar la imagen generada por Graphviz
    img = tk.PhotoImage(file=imagen_path)
    img = img.subsample(1, 1)  # Resize the image to half its original size
    # Limpiar cualquier imagen anterior
    label_imagen.config(image=img)
    label_imagen.image = img  # Mantener la referencia de la imagen

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
texto = tk.Text(window, height=40, width=60, yscrollcommand=scroll_texto.set)
scroll_texto.config(command=texto.yview)
texto.place(x=10, y=30)
scroll_texto.place(x=580, y=30, height=600)

# Etiqueta para mostrar la gráfica en la misma ventana
label_imagen = tk.Label(window)
label_imagen.place(x=600, y=30)

# Botón para ejecutar el análisis
tk.Button(window, text="Analizar", command=enviar_datos).pack()

# Ejecutar el bucle principal de la ventana
window.mainloop()