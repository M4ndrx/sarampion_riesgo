#!/usr/bin/env bash
set -euo pipefail

REPO_URL="https://github.com/IM-Data-PAHO/mr-risk-assessment-regional.git"
DEST_DIR="mr-risk-assessment-regional"

if [ -d "$DEST_DIR/.git" ]; then
  echo "La herramienta ya fue clonada en $DEST_DIR"
  exit 0
fi

if [ -d "$DEST_DIR" ] && [ "$(find "$DEST_DIR" -mindepth 1 -maxdepth 1 | wc -l)" -gt 0 ]; then
  echo "El directorio $DEST_DIR existe y no está vacío."
  exit 1
fi

rm -rf "$DEST_DIR"

git clone "$REPO_URL" "$DEST_DIR"

echo "Clone completado en $DEST_DIR"
