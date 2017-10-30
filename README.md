# CI4712
Laboratorio de Lenguajes de Programacion I
Proyecto I: Haskinator

Edwar Yepez 12-10855
Alessandra Marrero 12-11091

Instrucciones de uso:

1. Descargar los archivos, ejecutar el comando 'make' dentro del directorio donde se guardaron los archivos y así crear el ejecutable del programa.
2. Luego de compilar el programa, se ejecuta con ./haskinator en la terminal. 
3. Ingresar una opción válida del menú que se despliega para comunicarse con Haskinator. 

Detalles relevantes de la implementación:

- El programa es case sensitive, es decir, distingue entre mayúsculas y minúsculas. Debe poner sus respuestas exactamente como aparecen en pantalla o el programa le dirá respuesta inválida y le devolverá al menú inicial.
- Se utilizó deriving para las instancias de show y read y así no fue necesario realizar un parser. 
- Si el usuario ingresa una opción que no pertenece al menú se devuelve un error y se vuelve al menú de nuevo. 
- Si se selecciona predecir, persistir o consultar pregunta crucial cuando no se ha creado algún oráculo entonces también se devuelve un error que dice que no se ha inicializado un oráculo y se vuelve al menú inicial.  
- Cuando se está en predecir, al ingresar una opción, si el usuario se equivocó entonces devuelve error, se sale del predecir y vuelve al menú principal.
- Al escoger la opción de cargar, si se introduce un nombre de un archivo que no existe también se vuelve al menú luego de devolver un error. El archivo a cargar debe estar en el mismo directorio que los archivos del proyecto para que pueda ser leído y cargado. 

