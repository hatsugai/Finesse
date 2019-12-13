#include "object.h"
#include "builtin.h"

extern char init_path[];
extern vm_context vm;

int analize_options(int argc, char *argv[])
{
    int i = 1;
    while (i < argc) {
        if (strcmp(argv[i], "-d") == 0) {
            option_debug = true;
            i++;
        } else if (strcmp(argv[i], "-v") == 0) {
            option_verbose = true;
            i++;
        } else if (strcmp(argv[i], "-I") == 0) {
            if (i + 1 < argc) {
                strcpy(init_path, argv[i + 1]);
                size_t n = strlen(init_path);
                if (n > 0 && init_path[n - 1] != '/') {
                    strcat(init_path, "/");
                }
                i += 2;
            } else {
                fprintf(stderr, "invalid -I option\n");
                exit(1);
            }
        } else {
            break;
        }
    }
    return i;
}

int main(int argc, char *argv[])
{
    int i = analize_options(argc, argv);

    init_object_space();
    init_istack();
    init_environments();

    init_load("builtin.init", true);
    init_load("base.init", true);
    if (i == argc) {
        init_load("boot.init", true);
    } else {
        for ( ; i < argc; ++i) {
            init_load(argv[i], false);
        }
    }

    init_vm("main");
    opt result = execute();
    if (option_verbose) {
        write_sexpr(stdout, result);
        printf("\n");
    }

    if (is_fixnum(result)) {
        return fixnum_value(result);
    } else {
        return 1;
    }
}
