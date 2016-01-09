// Ejemplo (sin sentido) sobre opeadores aritmeticos con 6 errores
{
  int x[20];
  int i;
  bool b;
  read(b);             // El argumento del "read" debe ser "entero"
  while (i) {          // La expresion del "while" debe ser "logica"
    x[2] = i * b;      // Error de tipos en "expresion multiplicativa"
    x[2] = x[i] + b;   // Error de tipos en "expresion aditiva"
    i = x;             // El identificador debe ser de tipo simple
  }
  print(x[20] > i);    // La expresion del "print" debe ser "entera"
}
