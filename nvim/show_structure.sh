#!/bin/bash

# Función recursiva para recorrer directorios
function display_structure {
    local dir="$1"
    local indent="$2"

    # Mostrar el nombre de la carpeta actual
    echo "${indent}$(basename "$dir")/"

    # Recorrer los archivos y carpetas en el directorio actual
    for file in "$dir"/*; do
        if [ -d "$file" ]; then
            # Si es un directorio, llamamos a la función de forma recursiva
            display_structure "$file" "$indent  "
        else
            # Si es un archivo, mostramos su nombre y contenido
            echo "${indent}  $(basename "$file")"
            cat "$file"
            echo ""
        fi
    done
}

# Comenzar desde el directorio especificado o el directorio actual si no se especifica
start_dir="${1:-.}"
display_structure "$start_dir" ""