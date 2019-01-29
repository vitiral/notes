#include "common.h"
#include <assert.h>
#include <semaphore.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>  // file info

// return whether the strings are equal.
bool str_eq(char* s1, char* s2) { return strcmp(s1, s2) == 0; }

size_t path_join(char* path, size_t path_hi, char* with)
{
    size_t name_len = strlen(with);
    if (path_hi + name_len > MAX_PATH_LEN) {
        fprintf(stderr, "!! path too long: %s/%s\n", path, with);
        exit(1);
    }

    path[path_hi] = '/';
    strcpy(&path[path_hi + 1], with);
    size_t new_path_hi = path_hi      // $path
                         + 1          // '/'
                         + name_len;  // $name

    path[new_path_hi] = 0;
    return new_path_hi;
}

FType file_type(char* path, size_t path_hi)
{
    struct stat st;
    if (stat(path, &st)) {
        fprintf(stderr, "!! stat failed while reading %s\n", path);
        exit(1);
    }

    if ((st.st_mode & S_IFREG) && ends_with_txt(path, path_hi)) {
        return FTypeTxt;
    } else if (st.st_mode & S_IFDIR) {
        return FTypeDir;
    } else {
        return FTypeIgnore;
    }
}

// If `s` ends with ".txt" return true.
//
// s[hi] must == 0
bool ends_with_txt(char* s, size_t hi)
{
    if (hi - 1 < 4) {
        return false;
    }
    return str_eq(&s[hi - 4], ".txt");
}

// Trim the character from the end of the string. Return the new `hi`.
//
// `hi` points to the end of the string.
void trim_right(char* s, size_t* hi, char c)
{
    while (*hi > 1 && s[(*hi) - 1] == c) {
        (*hi)--;
    }
    s[*hi] = 0;
}

// Read up to `num` characters from the file, storing them in
// `buf`.
//
// Return the number of characters read. If this value is
// less than `num` then the file end has been reached.
size_t freadc(char* buf, size_t buf_size, FILE* stream)
{
    return fread(buf, 1, buf_size, stream);
}

sem_t* sem_new(unsigned int value)
{
    sem_t* out = malloc(sizeof(sem_t));
    if (out == NULL) {
        fprintf(stderr, "!! OOM\n");
        exit(1);
    }
    if (sem_init(out, /*pshared=*/0, value)) {
        fprintf(stderr, "!! sem_init FAILED\n");
        exit(1);
    }
    return out;
}

void sem_free(sem_t* sem)
{
    sem_destroy(sem);
    free(sem);
}
