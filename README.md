# TFM: Machine Learning para la predicción de Alzheimer

Este repositorio contiene los scripts de R y Python que se han creado para desarrollar un sistema de predicción con técnicas de Machine Learning de la enfermedad del Alzheimer a partir de la base de datos disponible en  [Alzheimer's Disease Dataset](https://www.kaggle.com/datasets/rabieelkharoua/alzheimers-disease-dataset).

## Autor
Marc Mejías Muñoz

## Requisitos
Para la elaboración del proyecto se han utilizado los lenguajes de programación R y Python. El script en R se ha ejecutado en RStudio version 2024.12.1+563 y para el script en Pyhton se ha utilizado la versión 3.11 desde Jupyter Notebook.

El script en R se ha utilizado para realizar el preprocesamiento de los datos, juntamente con los contrastes de hipotesis para encontrar las variables significativas. En este script se han generados los dos archivos correspendientes para posteriormente realizar los proceseos de Machine Learning.
El entorno completo para la ejecución del script en R esta en el archivo **requirements_r.txt**. Se puede configurar el entorno con los siguientes comandos en R:
```
packages <- readLines("requirements_r.txt")
install.packages(packages)
```

El script en Pyhton se ha utilizado para realizar los procesos de Machine Learning en los archivos previamente generados en el script en R.
El entorno completo para la ejecución del script en Python está en el archivo **requirements_py.txt**. Se puede configurar el entorno con los siguientes comandos en Pyhton:
```
pip install -r requirements_py.txt
```

