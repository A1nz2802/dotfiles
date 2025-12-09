#!/bin/bash

BOOK_DIR="/home/a1nz/stuff/books"

FILENAME=$(echo "$1" | sed 's/okular:\/\///')

okular "$BOOK_DIR/$FILENAME"
