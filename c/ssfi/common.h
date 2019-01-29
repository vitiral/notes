#ifndef COMMON_H
#define COMMON_H
#include <semaphore.h>
#include <stdbool.h>
#include <stdio.h>

#define MAX_PATH_LEN (1028)

enum FType_s {
    FTypeTxt,
    FTypeDir,
    FTypeIgnore,
};
typedef enum FType_s FType;

bool str_eq(char* s1, char* s2);
size_t path_join(char* path, size_t path_hi, char* with);
size_t freadc(char* buf, size_t buf_size, FILE* stream);
void trim_right(char* s, size_t* hi, char c);
FType file_type(char* path, size_t path_hi);
bool ends_with_txt(char* s, size_t hi);

sem_t* sem_new();
void sem_free(sem_t* sem);
#endif
