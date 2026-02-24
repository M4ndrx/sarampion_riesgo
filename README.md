# Sarampión Riesgo (país)

Repositorio de trabajo para organizar y cargar la información nacional que alimenta la herramienta regional de evaluación de riesgo de sarampión/rubéola.

> Este repositorio está preparado para que puedas ir incorporando datos por etapas y luego validarlos en iteraciones siguientes.

## Estructura actual del proyecto

```text
sarampion_riesgo/
├── mr-risk-assessment-regional/   # Carpeta reservada para la herramienta base
├── scripts/
│   └── clone_herramienta.sh       # Script para clonar la herramienta oficial
└── README.md
```

## Flujo recomendado

1. **Clonar la herramienta base** dentro de `mr-risk-assessment-regional/`.
2. **Cargar tu información nacional** en la estructura que estés usando en tu entorno.
3. **Validar calidad y consistencia** de los datos en una siguiente iteración.

## Clonado de la herramienta regional

El script incluido apunta al repositorio oficial:

- `https://github.com/IM-Data-PAHO/mr-risk-assessment-regional.git`

Ejecuta:

```bash
./scripts/clone_herramienta.sh
```

Si estás en un entorno sin salida a GitHub, el clon puede fallar por conectividad. En ese caso, simplemente ejecuta el script más adelante desde un entorno con acceso.

## Próxima iteración (validación)

Cuando termines de cargar datos, en la siguiente iteración te puedo ayudar a validar:

- campos obligatorios,
- formatos y tipos,
- consistencia temporal/geográfica,
- y reglas de negocio básicas para reducir errores antes del análisis.
