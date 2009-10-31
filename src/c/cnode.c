#include "erl_interface.h"

#include "extern.h"

#define BUFSIZE 1000

int main(int argc, char **argv) {
    int fd;

    int got;
    unsigned char buf[BUFSIZE];
    ErlMessage emsg;

    ETERM *fromp, *tuplep, *fnp, *argp, *resp;
    ETERM *res;
    /* res = erl_mk_atom("ok"); */

    erl_init(NULL, 0);

    read_all_maps("/home/alex/Projects/aliter/priv/maps");

    if (erl_connect_init(1, argv[2], 0) == -1)
        erl_err_quit("erl_connect_init");

    if ((fd = erl_connect(argv[1])) < 0) {
        debug("Connect failed. %d\n", fd);
        erl_err_quit("erl_connect");
    }

    while (1) {
        got = erl_receive_msg(fd, buf, BUFSIZE, &emsg);

        if (got == ERL_TICK)
            continue;

        if (got == ERL_ERROR) {
            debug("Got error from Erlang; exiting.\n");
            break;
        }

        if (emsg.type == ERL_REG_SEND) {
            fromp = erl_element(2, emsg.msg);
            tuplep = erl_element(3, emsg.msg);
            fnp = erl_element(1, tuplep);
            argp = erl_element(2, tuplep);

            if (strncmp(ERL_ATOM_PTR(fnp), "pathfind", 8) == 0) {
                res = pathfind(ERL_INT_VALUE(erl_element(1, argp)),
                               ERL_INT_VALUE(erl_element(2, argp)),
                               ERL_INT_VALUE(erl_element(3, argp)),
                               ERL_INT_VALUE(erl_element(4, argp)),
                               ERL_INT_VALUE(erl_element(5, argp)));

                resp = erl_format("{data, ~w}", res);
                erl_send(fd, fromp, resp);
                erl_free_term(resp);
            }

            erl_free_term(emsg.from);
            erl_free_term(emsg.msg);
            erl_free_term(fromp);
            erl_free_term(tuplep);
            erl_free_term(fnp);
            erl_free_term(argp);
        }
    }

    debug("C node exiting.\n");

    return 0;
}
