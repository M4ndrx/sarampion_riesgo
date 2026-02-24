# sarampion_riesgo
Riesgo Sarampión

## Preparación de la herramienta regional

Se dejó configurado el remoto `herramienta-origen` apuntando al repositorio oficial:

- `https://github.com/IM-Data-PAHO/mr-risk-assessment-regional.git`

Además, se agregó un script para clonar la herramienta dentro de este repositorio en la carpeta `mr-risk-assessment-regional/`:

```bash
./scripts/clone_herramienta.sh
```

> Nota: en este entorno de ejecución no hay salida de red a GitHub (respuesta `403`), por eso se dejó la automatización lista para ejecutarla apenas haya conectividad.
