# Proyecto 3 - Menú Saludable Inteligente

Sistema de recomendación de menús saludables usando Prolog como motor de inferencia y Python con Tkinter para la interfaz gráfica.

## Descripción

Este proyecto combina la programación lógica de Prolog con una interfaz gráfica en Python para generar menús saludables personalizados basados en las preferencias del usuario.

## Requisitos

- Docker
- Visual Studio Code
- Extensión "Dev Containers" para VS Code

## Configuración del Devcontainer

### Opción 1: Usando VS Code (Recomendado)

1. **Instalar la extensión Dev Containers en VS Code:**
   - Abrir VS Code
   - Ir a Extensions (Ctrl+Shift+X)
   - Buscar "Dev Containers" y instalar

2. **Abrir el proyecto en el devcontainer:**
   - Abrir la carpeta del proyecto en VS Code
   - Presionar `F1` o `Ctrl+Shift+P`
   - Escribir y seleccionar: `Dev Containers: Reopen in Container`
   - Esperar a que el contenedor se construya e inicie

3. **Ejecutar la aplicación:**
   ```bash
   python python/main.py
   ```

### Opción 2: Usando Docker CLI

1. **Construir la imagen del contenedor:**
   ```bash
   docker build -t menu-saludable -f .devcontainer/Dockerfile .
   ```

2. **Ejecutar el contenedor con soporte X11 (Linux):**
   ```bash
   xhost +local:docker
   docker run -it --rm \
     -v $(pwd):/workspace \
     -v /tmp/.X11-unix:/tmp/.X11-unix \
     -e DISPLAY=$DISPLAY \
     --network=host \
     menu-saludable bash
   ```

3. **Dentro del contenedor, ejecutar la aplicación:**
   ```bash
   python python/main.py
   ```

### Para WSL2 (Windows)

1. **Instalar un servidor X11 en Windows:**
   - Instalar [VcXsrv](https://sourceforge.net/projects/vcxsrv/) o [X410](https://x410.dev/)
   - Iniciar el servidor X11

2. **Configurar la variable DISPLAY en WSL2:**
   ```bash
   export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0
   ```

3. **Seguir los pasos de la Opci�n 1 o 2**



## Solución de Problemas

### Error: "no display name and no $DISPLAY environment variable"

Asegúrate de que:
1. Tienes un servidor X11 ejecutándose
2. La variable `DISPLAY` está correctamente configurada
3. El contenedor tiene permisos para acceder al X11 socket

### Error al conectar con Prolog

Verifica que el archivo `prolog/menu_saludable.pl` existe y está en la ubicación correcta.
