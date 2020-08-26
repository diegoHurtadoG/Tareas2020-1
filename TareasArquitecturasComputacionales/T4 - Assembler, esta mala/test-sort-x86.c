#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void sort_x86(char *noms[], int n);

// La estructura de una persona

char *noms[]= {"pedro gonzalez", "juan perez" , "diego gonzalez", "ana gonzalez", "vero fernandez", "jose perez", "monica fernandez", "alberto perez", "maria fernandez", "tatiana jerez"};

int main() {
  sort_x86(noms, 10);
  for (int i= 0; i<10; i++)
    puts(noms[i]);
  return 0;
}
