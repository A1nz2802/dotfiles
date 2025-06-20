# Atajos de teclado para `nvim-tree`

| Tecla   | Acción                                        | Comando Neovim                 |
|---------|-----------------------------------------------|--------------------------------|
| `a`     | Crear un archivo o carpeta                    | `:NvimTreeCreate<CR>`          |
| `r`     | Renombrar archivo o carpeta                   | `:NvimTreeRename<CR>`          |
| `d`     | Eliminar archivo o carpeta                    | `:NvimTreeRemove<CR>`          |
| `x`     | Cortar archivo o carpeta                      | `:NvimTreeCut<CR>`             |
| `c`     | Copiar archivo o carpeta                      | `:NvimTreeCopy<CR>`            |
| `p`     | Pegar archivo o carpeta copiado o cortado     | `:NvimTreePaste<CR>`           |
| `y`     | Copiar el nombre del archivo o carpeta        | `:NvimTreeCopyName<CR>`        |
| `Y`     | Copiar la ruta relativa del archivo o carpeta | `:NvimTreeCopyRelativePath<CR>`|
| `gy`    | Copiar la ruta absoluta del archivo o carpeta | `:NvimTreeCopyAbsolutePath<CR>`|
| `<C-h>`    | Copiar la ruta absoluta del archivo o carpeta | `:NvimTreeCopyAbsolutePath<CR>`|
| `<C-l>`    | Copiar la ruta absoluta del archivo o carpeta | `:NvimTreeCopyAbsolutePath<CR>`|
| `<C-h>`     | Desplazarse a la ventana de la derecha        | `:NvimTreeNavigateRight<CR>`   |
| `<C-l>`     | Desplazarse a la ventana de la izquierda      | `:NvimTreeNavigateLeft<CR>`    |

# Configuración de Atajos de Teclado para `barbar.nvim`

| Tecla               | Acción                                         | Comando Neovim           |
|---------------------|------------------------------------------------|---------------------------|
| `<TAB>`             | Mover al siguiente buffer                     | `:BufferNext<CR>`         |
| `<S-TAB>`           | Mover al buffer anterior                       | `:BufferPrevious<CR>`     |
| `<C-b>`             | Cerrar el buffer actual                        | `:BufferClose<CR>`        |
| `<A-,>`             | Mover el buffer actual a la izquierda en la lista | `:BufferMovePrevious<CR>` |
| `<A-.>`             | Mover el buffer actual a la derecha en la lista  | `:BufferMoveNext<CR>`     |
| `<A-p>`             | Fijar el buffer actual (pin)                   | `:BufferPin<CR>`          |
| `<leader>b`         | Cerrar todos los buffers excepto el actual    | `:BufferCloseAllButCurrent<CR>` |

> **Nota**: `<leader>` se refiere a la tecla líder configurada en tu Neovim. Por defecto, esta tecla es el espacio (` `).

| **Tecla**         | **Modo** | **Acción**                                           |
|-------------------|----------|------------------------------------------------------|
| `<leader>e`       | Normal   | Alterna la visibilidad del árbol de archivos (NvimTree) |
| `<C-s>`           | Normal   | Guarda el archivo actual                             |
| `<C-s>`           | Insert   | Guarda el archivo actual y vuelve al modo Normal     |
| `jk`              | Insert   | Salir del modo Insert                                |
| `kj`              | Insert   | Salir del modo Insert                                |
| `<TAB>`           | Visual   | Indenta a la derecha                                 |
| `<S-TAB>`         | Visual   | Indenta a la izquierda                               |
| `>`               | Visual   | Indenta a la derecha                                |
| `<`               | Visual   | Indenta a la izquierda                              |
| `K`               | Visual   | Mueve la línea seleccionada hacia arriba             |
| `J`               | Visual   | Mueve la línea seleccionada hacia abajo             |
| `<C-l>`           | Normal   | Mueve a la ventana de la derecha                     |
| `<C-h>`           | Normal   | Mueve a la ventana de la izquierda                   |
| `<C-j>`           | Normal   | Mueve a la ventana de abajo                          |
| `<C-k>`           | Normal   | Mueve a la ventana de arriba                         |
| `<M-h>`           | Normal   | Aumenta el ancho de la ventana vertical              |
| `<M-l>`           | Normal   | Disminuye el ancho de la ventana vertical            |
| `<M-j>`           | Normal   | Aumenta la altura de la ventana horizontal           |
| `<M-k>`           | Normal   | Disminuye la altura de la ventana horizontal         |
| `<leader>p`       | Visual   | Pega el contenido del portapapeles sobre la selección sin perder el contenido del portapapeles |
| `<leader>y`       | Normal   | Copia la selección al portapapeles                   |
| `<leader>y`       | Visual   | Copia la selección al portapapeles                   |
| `<leader>Y`       | Normal   | Copia la línea actual al portapapeles                |
| `<leader>r`       | Normal   | Reemplaza todas las instancias de la palabra bajo el cursor con una nueva palabra |
| `<leader>r`       | Visual   | Reemplaza todas las instancias de la selección con una nueva palabra |
| `<leader>s`       | Normal   | Destaca el texto buscado y luego borra la búsqueda     |
| `<leader>s`       | Visual   | Destaca el texto seleccionado y luego borra la búsqueda |
| `<leader>t`       | Normal   | Abre una terminal en una ventana dividida en la parte inferior |
| `<C-n>`           | Terminal | Regresa al modo Normal desde el modo Terminal        |
| `<C-k>`           | Terminal | Regresa al modo Normal y mueve el cursor a la ventana de arriba |
