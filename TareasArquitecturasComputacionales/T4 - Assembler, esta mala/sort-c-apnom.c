#include <string.h>

void sort_x86_apnom(char noms, int n) {
    char ult= &noms[n-1];
    char **p= noms;
    int count;
    int i, j, k;
    char apellido1[50];
    char apellido2[50];
    while (p<ult) {
        count = 0;
        k = 0;
        j = 0;
        for(i=0; i<strlen(p[0]); i++){
            if(p[0][i] == ' '){
                count = i;
            }
            if(count != 0){
                apellido1[j++] = p[0][i];
            }
        }
        apellido1[strlen(p[0]) - count] = '\0';
        count = 0;
        for(i=0; i<strlen(p[1]); i++){
            if(p[1][i] == ' '){
                count = i;
            }
            if(count != 0){
                apellido2[k++] = p[1][i];
            }
        }
        apellido2[strlen(p[1]) - count] = '\0';
        if (strcmp(apellido1, apellido2)<0) {
            p++;
        }
        else if((strcmp(apellido1, apellido2)==0) && (strcmp(p[0], p[1])<=0)) {
            p++;
        }
        else{
            char *tmp= p[0];
            p[0]= p[1];
            p[1]= tmp;
            p = noms;
        }
    }
}