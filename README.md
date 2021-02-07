# detector-gen-mutante

version 2 de detector gen mutante

# pre-requisitos

* Se debe tener instalado java version 11
* se debe tener instalado [Node](https://nodejs.org) y [NPM](hhttps://www.npmjs.com/)
* Se debe tener instalado gradle
* Tener Cuenta en Amazon AWS con los siguientes servicios
    1. Lambda function creada(se ddeben tener 2 creadas una Java y la otra en node JS)
    2. API gateway creado con los recursos necesarios para llamar a la lambda
    3. DynamoDB tabla creada llamada "registro-gen" con: 
        -La clave de partición principal "dna"
        -La clave de ordenación principal "esMutante"
    4. SQS Cola de procesos para encolar las peticiones de guardado 

# generacion de Zip Java

Esta aplicacion esta construida en Java y para su despliegue primero se debe generar un zip con sus compilados

El comando para ejecutarlo es

 gradle clean build

generara un zip detector-gen-mutante-0.0.1-SNAPSHOT.zip en la ruta del proyecto /build/distrtibutions/

# despliegue Lambda Java

En la cuenta de Amazon AWS se debe adjuntar el zip generado en el paso anterior

en la "Configuración del tiempo de ejecución" de la lambda en el controlador se debe editar y 
colocar "detector.gen.mutante.aws.function.StartProcessRequestHandler" 

# generacion de Zip Node

Los proyectos de node se en cuentra en: 

*  (https://github.com/kmiloparra/guardar-analisis-gen-mutante)

*  (https://github.com/kmiloparra/estadisticas-gen-mutante)

Los comandos para obtener ambos zip son

 npm install

 npm run build

Se generara un zip en la ruta de los proyectos en /dist/

con los zip llamados:

-guardar-analisis-gen-mutante.zip
-estadisticas-gen-mutante.zip

# despliegue Lambdas node

En la cuenta de Amazon AWS se debe adjuntar los zip generados en el paso anterior

en la "Configuración del tiempo de ejecución" de las lambdas en el controlador se debe editar y 
colocar "src/index.handler" 


**Ejemplo Detector Mutante**

* [endpoint definido en el apigateway]/mutant 
* POST

```

{
    "dna":["GTCGAGTA","TCGAGTAG","CGATTAGT","GAGAGGTC","GAGAGGTC","AGAGTCGT","AGAGTCGT","AGAGTCGT"]
}

```

**Ejemplo Estadisticas Mutante**

* [endpoint definido en el apigateway]/stats 
* GET

No Body

## Arquitectura De la aplicacion

![imagen perdida] (https://ibb.co/HrCKzsS)
