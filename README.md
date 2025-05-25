# TFM: Machine Learning para la predicción de Alzheimer

Este repositorio contiene los scripts de **R** y **Python** que se han creado para desarrollar un sistema de predicción de la enfermedad del Alzheimer utilizando técnicas de Machine Learning a partir de la base de datos disponible en  [Alzheimer's Disease Dataset](https://www.kaggle.com/datasets/rabieelkharoua/alzheimers-disease-dataset).

## Autor
Marc Mejías Muñoz

## Información institucional
* Universitat Oberta de Catalunya (UOC).
* Máster Universitario de Data Science.

## Requisitos
Para la elaboración del proyecto se han utilizado los lenguajes de programación R y Python:
* El script en **R** se ha ejecutado en *RStudio version 2024.12.1+563*. Ha sido utilizado para el preprocesamiento de datos y análisis estadístico (contrastes de hipótesis), y genera los archivos preparados para el modelado en Python.
* El script en **Python** se ha ejecutado sobre *Python 3.11* en un entorno *Jupyter Notebook*. Se ha empleado para construir, entrenar y evaluar los modelos de Machine Learning.


El entorno completo para la ejecución del script en R está en el archivo **requirements_r.txt**. Para instalar las dependencias, ejecutar en R:
```
packages <- readLines("requirements_r.txt")
install.packages(packages)
```

El entorno completo para la ejecución del script en Python está en el archivo **requirements_py.txt**. Se puede configurar el entorno con los siguientes comandos en Pyhton:
```
pip install -r requirements_py.txt
```

## Contenido
* **/source/M2980_Mejias_Marc_TFM.R**: Script con el preprocesamiento, análisis exploratorio de los datos y contrastes de hipótesis.
* **/source/M2980_Mejias_Marc_TFM.ipynb**: Script con los procesos de Machine Learning.
* **/requirements/requirements_r.txt**: Librerías que forman el entorno de ejecución en R.
* **/requirements/requirements_py.txt**: Librerías y versiones que forman el entorno de ejecución en Python.

## Licencia
Esta obra está sujeta a una licencia de Reconocimiento - NoComercial - SinObraDerivada [3.0 España de CreativeCommons](https://creativecommons.org/licenses/by-nc-nd/3.0/es/)

