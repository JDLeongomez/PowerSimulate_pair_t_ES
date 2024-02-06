# <img src="https://github.com/JDLeongomez/PowerSimulate_ind_t_EN/blob/master/www/powersimulate.svg" align="left" width=400 height=100 alt=""/>

## Prueba *t* pareada
<br>
Análisis de poder basado en la simulación de una población y en la probabilidad de obtener un resultado significativo con una muestra de un tamaño determinado.
Aunque existen herramientas más directas para el análisis de poder en el caso de las pruebas t, esta aplicación se basa en simulaciones para ilustrar el concepto de poder estadístico

<hr>

Esta aplicación está disponible en mi (bastante lento) [servidor personal Shiny](https://shiny.jdl-svr.lat/PowerSimulate_ind_t_ES/). Sin embargo, si corre demasiado lento o mi servidor no funciona, siempre puedes ejecutarla localmente en tu ordenador con R instalado. 

Para ello, basta con ejecutar el código siguiente en R:

```R
library(shiny)
runGitHub("PowerSimulate_pair_t_ES", "JDLeongomez")
```

(**NOTE:** For the English version, go to https://github.com/JDLeongomez/PowerSimulate_pair_t_EN)
